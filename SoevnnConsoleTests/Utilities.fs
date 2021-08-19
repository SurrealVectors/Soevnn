module Soevnn.Console.Utilities

open System
open Soevnn.Core
open Soevnn.Parallel.Core
open System.IO
open MessagePack
open Soevnn.Serialization
open Microsoft.FSharp.Reflection

let IsEqual a b = a = b
let IsAny l v = List.exists (IsEqual v) l
let SomeIfForAll c l = if (List.forall c l) then Some l else None
let IndexInRange (l : _ list) i = i >= 0 && i < l.Length
let curry2 f a b = f (a,b)
let uncurry2 f (a,b) = f a b
let TryResultAsOption (didsucceed, result : 'r) = 
    if didsucceed then Some result else None

/// <summary> Provides experiment data for a single Soevnn. </summary>
[<MessagePackObject>]
type SoevnnTestData(majorversion, minorversion, revision, iteration, maxiteration, samplecount, samples, uptime) =
    [<Key(0)>]
    member _.MajorVersion : int = majorversion
    [<Key(1)>]
    member _.MinorVersion : int = minorversion
    [<Key(2)>]
    member _.RevisionNumber : int = revision
    [<Key(3)>]
    member _.Iteration : int = iteration
    [<Key(4)>]
    member _.MaxIteration : int option = maxiteration
    [<Key(5)>]
    member _.SampleCount : int = samplecount
    [<Key(6)>]
    member _.Samples : float array System.Collections.Generic.Queue = samples
    [<Key(7)>]
    member _.UpTime : int64 = uptime



/// <summary> Provides some basic formatting for console text, including word wrap and offsets. </summary>
let rec formattext maxwidth starting offset (text:string) = 
    let offsettext : string = // Adds spaces as necessary to reach the intended offset.
        if offset > starting then System.String([|for i in [1..offset-starting] do yield ' '|]) else ""
    let offsettextlength = offsettext.Length
    if text = "" then // There's nothing left to process, so return the result.
        ("",starting)
    else if starting + offsettextlength + text.Length < maxwidth then // This is the last line to process, so return the result.
        (offsettext + text, starting + offsettextlength + text.Length)
    else
        let getnewline = // Otherwise, look for the next newline.
            text
            |> Seq.tryFindIndex
                ((=) '\n')
        match getnewline with
        | Some pos ->
            if pos = 0 then
                let resultb, column = formattext maxwidth 0 offset text.[pos+1..text.Length-1]
                ("\n" + resultb, column)
            else
                let resulta, _ = formattext maxwidth starting offset text.[0..pos-1] 
                let resultb, column = formattext maxwidth 0 offset text.[pos+1..text.Length-1]
                (resulta + "\n" + resultb, column)
        | _ ->
            if starting + offsettext.Length >= maxwidth then
                let resultb,column = formattext maxwidth 0 offset text
                ("\n" + resultb, column)
            else
                let linebreakpos =
                    seq {for i in [0.. maxwidth - starting - offsettextlength - 1] do yield text.[i]}
                    |> Seq.tryFindIndexBack ((=) ' ')
                match linebreakpos with
                | Some pos ->
                    let resultb,column = formattext maxwidth 0 offset text.[pos+1..text.Length-1]
                    (offsettext + text.[0..pos-1] + "\n" + resultb, column)
                | None ->
                    let pos = maxwidth - starting - offsettextlength - 1
                    let resultb,column = formattext maxwidth 0 offset text.[pos+1..text.Length-1]
                    (offsettext + text.[0..pos] + "\n" + resultb, column)
// TODO: Replace mentions of "Line" with "Paragraph", as that is a more accurate description.
/// <summary> A concurrent, reformatable print buffer. </summary>
type PrintBuffer(text:string) =
    let previouslines = new System.Collections.Concurrent.ConcurrentStack<int * string>()
    let currentline = new System.Collections.Concurrent.ConcurrentStack<(int * int) * string>() // unformatted ending column * formatted ending column * text
    let getoffset (text : string) =
        let rec getoffset (text : string) counter =
            if text.Length > counter && text.[counter] = ' ' then getoffset text (counter+1) else counter
        if text.Length > 0 && text.[0] = ' ' then getoffset text 1 else 0
    let print (text : string) (offset : int option) (newline : bool) (printtoconsole : bool) =
        let previouslength, previousstarting = currentline.TryPeek() |> TryResultAsOption |> Option.map fst |> Option.defaultValue (0,0)
        let thisline = (currentline.ToArray() |> Array.rev |> Array.map snd |> String.concat "")
        let offset =
            offset
            |> Option.defaultValue (getoffset thisline)
                    
        let offsettext : string =
            if offset > previouslength then System.String([|for i in [1..offset-previouslength] do yield ' '|]) else ""
        let formattedtext, column =
            formattext
                Console.WindowWidth
                previouslength
                offset
                text
        currentline.Push((previouslength + offsettext.Length + text.Length, column), offsettext + text)
        if newline then
            if printtoconsole then Console.WriteLine(formattedtext)
            let thisline = thisline + offsettext + text
            previouslines.Push(getoffset thisline, thisline.TrimStart(' '))
            currentline.Clear()
        else
            if printtoconsole then Console.Write(formattedtext)
    do
        if text.Length > 0 then
            let lines = text.Split('\n')
            for line in lines do
                let offset = getoffset line
                previouslines.Push((offset,line.Substring(offset)))
    new() = PrintBuffer("")
    /// <summary> Returns the contents of the buffer as an unformatted string. </summary>
    member _.GetUnformattedString() =
        (
            previouslines.ToArray()
            |> Array.rev
            |> Array.map (fun (offset,text) -> String.replicate offset " " + text)
            |> String.concat "\n"
        )
        +
        "\n"
        +
        (
            currentline.ToArray()
            |> Array.rev
            |> Array.map snd
            |> String.concat ""
        )
    /// <summary> Returns the contents of the buffer as a formatted string. </summary>
    member _.GetFormattedString(maxwidth) =
        (
            previouslines.ToArray()
            |> Array.rev
            |> Array.map (formattext maxwidth 0 |> uncurry2 >> fst)
            |> String.concat "\n"
        )
        +
        "\n"
        +
        (
            currentline.ToArray()
            |> Array.rev
            |> Array.map snd
            |> String.concat ""
            |> formattext maxwidth 0 0
            |> fst
        )
    /// <summary> Prints a line of text to the buffer. </summary>
    member _.PrintLine(text : string) =
        print text None true true
    /// <summary> Prints a line of text to the buffer, with a minimum offset from the start of the line. </summary>
    member _.PrintLine(text : string, offset:int) =
        print text (Some offset) true true
    /// <summary> Adds a read line of text to the buffer. </summary>
    member _.ReadLine(text : string) =
        print text None true false
    /// <summary> Prints some text to the buffer. </summary>
    member _.Print(text : string) =
        print text None false true
    /// <summary> Prints some text to the buffer, with a minimum offset from the start of the line. <summary>
    member _.Print(text : string, offset:int) =
        print text (Some offset) false true
    /// <summary> Adds some read text to the buffer. </summary>
    member _.Read(text : string) =
        print text None false false

/// <summary> Provides the mechanism for typed arguments for formatted text like printf has. </summary>
let printc (printer : string * obj list -> 'r) (format : PrintfFormat<'p, _, _, 'r>) : 'p =
    if not <| FSharpType.IsFunction typeof<'p> then // Checks if the format is just a basic string with no formatting. 
        (format.Value,[]) // If so, just use the printer on the text of the format.
        |> printer
        |> unbox
    else // Otherwise create a types functions of type 'p that will accept the parameters indicated in the format.
        let rec flattenfunctiontypes (functionType: Type) = // Get the types of a curried function.
            let domain, range = FSharpType.GetFunctionElements functionType 
            if not <| FSharpType.IsFunction range then 
                domain::[range] 
            else 
                domain::flattenfunctiontypes(range) 
        let types = flattenfunctiontypes typeof<'p> // The types of the function.
        let rec proc (types: Type list) (values: obj list) (a: obj) : obj = 
            let values = a::values // The parameter values.
            match types with 
            | [_;_] -> // Has the expected number of parameter values to execute the function, so do that.
                let result = printer (format.Value, List.rev values) 
                box result 
            | _::d::r::tl -> // Otherwise, create a curried function that will continue this process.
                let cont = proc (r::d::tl) values 
                let ft = FSharpType.MakeFunctionType(d,r) 
                let cont = FSharpValue.MakeFunction(ft, cont) 
                box cont 
            | _ -> failwith "shouldn't happen" // TODO: Add proper error message here. But also, yes, this really shouldn't happen.
        unbox <| FSharpValue.MakeFunction(typeof<'p>, proc types []) // Create the function and unbox it to support proper type safety.
        
/// <summary> Replaces symbols in formatted text with the typed arguments. </summary>
let replaceformatting (s : string) (objects : obj list) =
    let i = ref 0
    let eval (rxMatch: System.Text.RegularExpressions.Match) =
        match (rxMatch.Value,objects.[!i]) with
        | ("%s",(:? string as value)) -> incr i; sprintf "%s" value
        | ("%i",(:? int as value)) -> incr i; sprintf "%i" value
        | ("%f",(:? float as value)) -> incr i; sprintf "%f" value
        | ("%b",(:? bool as value)) -> incr i; sprintf "%b" value
        | ("%%",_) -> "%"
        | (_,value) -> incr i; sprintf "%A" value
    System.Text.RegularExpressions.Regex.Replace(s, "%([sifb%])", eval)

/// <summary> Writes to a PrintBuffer with a newline. </summary>
let bnprinter (buffer : PrintBuffer) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.PrintLine(text)

/// <summary> Writes to a PrintBuffer with a newline and offset. </summary>
let bnfprinter (buffer : PrintBuffer) (offset : int) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.PrintLine(text,offset)

/// <summary> Writes to a PrintBuffer. </summary>
let bprinter (buffer : PrintBuffer) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.Print(text)

/// <summary> Writes to a PrintBuffer with an offset. </summary>
let bfprinter (buffer : PrintBuffer) (offset : int) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.Print(text, offset)

/// <summary> Reads a line from a PrintBuffer. </summary>
let bnrprinter (buffer : PrintBuffer) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.ReadLine(text)

// Provides a specific print buffer to be used for the console program, along with print and read functions for it.
let buffer = new PrintBuffer()
/// <summary> Writes to the buffer with a newline. </summary>
let printbn<'p> : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bnprinter buffer)
/// <summary> Writes to the buffer with a newline and offset. </summary>
let printb<'p> : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bprinter buffer)
/// <summary> Writes to the buffer with an offset. </summary>
let printbnf<'p> offset : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bnfprinter buffer offset)
/// <summary> Writes to the buffer with an offset. </summary>
let printbf<'p> offset : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bfprinter buffer offset)
/// <summary> Reads a line from the buffer. </summary>
let readbn<'p> : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bnrprinter buffer)


