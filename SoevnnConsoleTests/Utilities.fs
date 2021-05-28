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




let rec formattext maxwidth starting offset (text:string) = 
    let offsettext : string =
        if offset > starting then System.String([|for i in [1..offset-starting] do yield ' '|]) else ""
    let offsettextlength = offsettext.Length
    if text = "" then
        ("",starting)
    else if starting + offsettextlength + text.Length < maxwidth then
        (offsettext + text, starting + offsettextlength + text.Length)
    else
        let getnewline =
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
    member _.PrintLine(text : string) =
        print text None true true
    member _.PrintLine(text : string, offset:int) =
        print text (Some offset) true true
    member _.ReadLine(text : string) =
        print text None true false
    member _.Print(text : string) =
        print text None false true
    member _.Print(text : string, offset:int) =
        print text (Some offset) false true
    member _.Read(text : string) =
        print text None false false

let printc (printer : string * obj list -> 'r) (format : PrintfFormat<'p, _, _, 'r>) : 'p =
    if not <| FSharpType.IsFunction typeof<'p> then
        (format.Value,[])
        |> printer
        |> unbox
    else
        let rec flattenfunctiontypes (functionType: Type) = 
            let domain, range = FSharpType.GetFunctionElements functionType 
            if not <| FSharpType.IsFunction range then 
                domain::[range] 
            else 
                domain::flattenfunctiontypes(range) 
        let types = flattenfunctiontypes typeof<'p> 
        let rec proc (types: Type list) (values: obj list) (a: obj) : obj = 
            let values = a::values 
            match types with 
            | [_;_] -> 
                let result = printer (format.Value, List.rev values) 
                box result 
            | _::d::r::tl -> 
                let cont = proc (r::d::tl) values 
                let ft = FSharpType.MakeFunctionType(d,r) 
                let cont = FSharpValue.MakeFunction(ft, cont) 
                box cont 
            | _ -> failwith "shouldn't happen" 
        unbox <| FSharpValue.MakeFunction(typeof<'p>, proc types [])
        

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

let bnprinter (buffer : PrintBuffer) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.PrintLine(text)
let bnfprinter (buffer : PrintBuffer) (offset : int) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.PrintLine(text,offset)
let bprinter (buffer : PrintBuffer) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.Print(text)
let bfprinter (buffer : PrintBuffer) (offset : int) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.Print(text, offset)
let bnrprinter (buffer : PrintBuffer) (text : string, objects : obj list) =
    let text = replaceformatting text objects
    buffer.ReadLine(text)


let buffer = new PrintBuffer()
let printbn<'p> : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bnprinter buffer)
let printb<'p> : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bprinter buffer)
let printbnf<'p> offset : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bnfprinter buffer offset)
let printbf<'p> offset : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bfprinter buffer offset)
let readbn<'p> : PrintfFormat<'p,unit,unit,unit> -> 'p = printc (bnrprinter buffer)


