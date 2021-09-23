module Soevnn.Console.Program

open System
open Soevnn.Core
open Soevnn.Parallel.Core
open System.IO
open Soevnn.Serialization
open Soevnn.Console.Utilities
open Soevnn.Console.Testing





let populations = 
    [
        // intrainterbalancegroup, intrainterbalancestructure, intrainterbalancecluster, directinversebalance, clustersize, clustercount, currentaccumrate, expectedaccumrate, adaptionrate, synapsecountmin, synapsecountmax
      
      // GRP    STR     CLST    INV   CLU-SIZ   CRATE   ERATE   ARATE  SYN-MIN  SYN-MAX PATH
        (0.5,   0.1,    0.3,    0.1,    20,     0.9,    0.03,   0.4,    8,      30,     0)
        (0.5,   0.1,    0.3,    0.1,    18,     1.0,    0.04,   0.2,    3,      9,      0)
        (0.3,   0.3,    0.3,    0.1,    6,      0.7,    0.05,   0.07,   8,      24,     1)
        (0.8,   0.1,    0.8,    0.8,    9,      0.9,    0.1,    0.01,   1,      6,      2)
        (0.3,   0.3,    0.3,    0.1,    40,     1.0,    0.03,   0.04,   3,      9,      2)
        (0.5,   0.5,    0.5,    0.1,    9,      1.3,    0.05,   0.02,   3,      17,     2)
        (0.4,   0.5,    0.5,    0.3,    20,     0.5,    0.1,    0.02,   1,      5,      3)
    ]

let initialstructures = 
    [ // list of construction data to form neural structures
        [ // list of populations that make up a single structure
            (3,4) // (how many of, the population at this index)
            (3,5)
            (2,6)
        ]
        [
            (4,5)
            (2,3)
        ]
        [
            (2,0)
            (3,1)
            (2,2)
        ]
    ]

let (|Command|_|) (name:string list) (values : string list) = // Active pattern to match a command.
    match values with
    | head::tail -> if IsAny name head then Some tail else None
    | [] -> None

type ParamType = // The possible types of command parameters. 
| PtInt
| PtFloat
| PtString
| PtUnit
| PtList of ParamType

let rec (|Commands|_|) (commands: (string list * (string list * ParamType * string) list * string * (_ -> _)) list) (values : string list) = // Active pattern to match a command against a list of possible commands.
    match commands with
    | (name,paraminfolist,info,dothis) :: ctail ->
        match values with
        | head::tail -> if IsAny name head then Some (tail,dothis) else ``|Commands|_|`` ctail values
        | [] -> ``|Commands|_|`` ctail values
    | [] -> None

let ExtractInt (value:string) : int option = // Tries to get the int value of the parameter.
    let result = ref 0
    if System.Int32.TryParse(value, result) then
        Some result.Value
    else
        None

let ExtractFloat (value:string) : float option = // Tries to get the float value of the parameter.
    let result = ref 0.0
    if System.Double.TryParse(value, result) then
        Some result.Value
    else
        None

let ExtractArray<'item> (valueextractor : string -> 'item option) (value : string) = // Tries to get a list value of the parameter.
    if value.[0] = '[' && value.[value.Length-1] = ']' then
        let rec split (remargs : char list) (partial : char list) (results : string list) (isinquote : bool) (parenslayer : int) =
            match remargs with
            | ',' :: tail -> if isinquote || parenslayer > 0 then split tail (','::partial) (results) true parenslayer else split tail [] ((partial|>List.rev|>string) :: results) false parenslayer
            | ' ' :: tail -> if isinquote then split tail (' '::partial) (results) true parenslayer else split tail partial results false parenslayer
            | '\"' :: tail -> split tail partial results (not isinquote) parenslayer
            | '[' :: tail -> if isinquote then split tail ('['::partial) results isinquote parenslayer else split tail ('['::partial) results isinquote (parenslayer+1)
            | ']' :: tail -> if isinquote then split tail (']'::partial) results isinquote parenslayer else split tail (']'::partial) results isinquote (parenslayer-1)
            | c :: tail -> split tail (c :: partial) results isinquote parenslayer
            | [] -> results |> List.rev
        split ((value.ToCharArray(), 1, (value.Length-2)) |||> Array.sub |> List.ofArray) [] [] false 0
        |> List.choose valueextractor
        |> Some
    else
        None

let ExtractPopulation (floatextractor : string -> float option) (intextractor : string -> int option) (args : string list) = // Tries to get a population value of the parameter.
    let argcounter = ref 0
    let getarg() = argcounter := argcounter.Value + 1; args.[argcounter.Value - 1]
    if args.Length = 10 then
        match (floatextractor <| getarg(), floatextractor <| getarg(), floatextractor <| getarg(), floatextractor <| getarg(), intextractor <| getarg(), floatextractor <| getarg(), floatextractor <| getarg(), floatextractor <| getarg(), intextractor <| getarg(), intextractor <| getarg(), intextractor <| getarg()) with
        | (Some intrainterbalancegroup, Some intrainterbalancestructure, Some intrainterbalancecluster, Some directinversebalance,Some clustersize, Some currentaccumrate, Some expectedaccumrate, Some adaptionrate, Some synapsecountmin, Some synapsecountmax, Some pathway) ->
            Some (intrainterbalancegroup, intrainterbalancestructure, intrainterbalancecluster, directinversebalance, clustersize, currentaccumrate, expectedaccumrate, adaptionrate, synapsecountmin, synapsecountmax, pathway)
        | _ -> None
    else
        None
    



let defaultoption (defaultoption : 'a) (option : 'a option)  =
    if option.IsSome then option.Value else defaultoption

let extractarg (argname : string list) (argtype : string->'result option) (args : string list) : 'result option = // Tries to get the parameter of an expected type.
    if List.exists (IsAny argname) args then
        let index = List.findIndex (IsAny argname) args
        printbn "Given argument %s" argname.[1]
        (args.[1+ index])
        |> argtype
    else
        None
    

let extractargs (argname : string list) (argtype : string->'result option) (args : string list) : 'result list = // Tries to get then parameters of and expected type.
    if List.exists (IsAny argname) args then
        let results = [for i in [0 .. args.Length - 2] do if IsAny argname args.[i] then match argtype args.[i+1] with | Some value -> yield! [value] | None -> ()  ]
        printbn "Given argument %s" argname.[1]
        results
    else
        []    
    
let getargs (args : string) = // Gets the list of arguments in string format from a single string.
    let stringofcharlist (cl : char list) = (cl |> List.rev |> List.map string |> String.concat "")
    let rec split (remargs : char list) (partial : char list) (results : string list) (isinquote : bool) (parensdepth : int) =
        match remargs with
        | ' ' :: tail -> if isinquote || parensdepth > 0 then split tail (' '::partial) (results) true parensdepth else split tail [] ((partial|>stringofcharlist) :: results) false parensdepth
        | '"' :: tail -> split tail partial results (not isinquote) parensdepth
        | '[' :: tail -> if isinquote then split tail ('[' :: partial) results isinquote parensdepth else split tail ('[' :: partial) results isinquote (parensdepth + 1)
        | ']' :: tail -> if isinquote then split tail (']' :: partial) results isinquote parensdepth else split tail (']' :: partial) results isinquote (parensdepth - 1)
        | c :: tail -> split tail (c :: partial) results isinquote parensdepth
        | [] -> (partial |> stringofcharlist) :: results |> List.rev
    split (args.ToCharArray() |> List.ofArray) [] [] false 0
    |> List.filter (IsEqual "" >> not)

type CommandMessage<'test> = // Possible program commands.
    | CommandArgs of (string list -> unit) * string list
    | Test of 'test
    | Stop

type CommandState = // Unused. TODO: Remove this.
    | Continue
    | StopState
    | Error of string

let timespantostring (etl:TimeSpan) = // Formats a timespan as a string to be appropriately specific.
    if etl.TotalDays > 1.0 then 
        etl.Days.ToString() + " Days, " + etl.Hours.ToString() + " Hours"
    else if etl.TotalHours > 1.0 then 
        etl.Hours.ToString() + " Hours, " + etl.Minutes.ToString() + " Minutes"
    else if etl.TotalMinutes > 1.0 then 
        etl.Minutes.ToString() + " Minutes, " +  etl.Seconds.ToString() + " Seconds"
    else if etl.TotalSeconds > 1.0 then 
        etl.Seconds.ToString() + " Seconds"
    else
        (etl.TotalMilliseconds |> int |> string) + " Milliseconds"



[<EntryPoint>]
let main argv =
    Console.BufferWidth <- Console.LargestWindowWidth

    let messagequeue = new System.Collections.Concurrent.ConcurrentQueue<string>()
    let testcommandqueue = new MailboxProcessor<TestCommand>(fun mp -> async { () })
    let tests = System.Collections.Concurrent.ConcurrentDictionary<int,System.Collections.Concurrent.ConcurrentQueue<string>*MailboxProcessor<TestCommand>>()
    let testnames = System.Collections.Concurrent.ConcurrentDictionary<string,int>()
    let cancelcommandqueue = System.Threading.CancellationToken()
    

    use commandqueue = 
        new MailboxProcessor<CommandMessage<_>>(
            (fun mp ->
                let rec loop state =
                    async{
                        let! commsg = mp.Receive()
                        match commsg with
                        | CommandArgs(comproc, comargs) -> // Execute a console command with the given arguments.
                            comproc comargs
                            return! loop Continue
                        | Test(populations, structures, seed, trainingsteps, testingsteps, (fname, inputfunction), logfile, silent, messagequeue, testcommandqueue) -> // Initiate a test with the given parameters.
                            try
                                if tests.TryAdd(tests.Count, ((match messagequeue with | Some value -> value | None -> new System.Collections.Concurrent.ConcurrentQueue<string>()),match testcommandqueue with | Some value -> value | None -> new MailboxProcessor<TestCommand>(fun mp -> async { () }))) then
                                    Async.Start(async { TestNervousSystem populations structures seed trainingsteps testingsteps (fname, inputfunction) logfile silent (tests.[tests.Count-1] |> fst |> Some) (tests.[tests.Count-1] |> snd |> Some)}) 
                                else
                                    TestNervousSystem populations structures seed trainingsteps testingsteps (fname, inputfunction) logfile silent messagequeue testcommandqueue
                                return! loop Continue
                            with
                            | e -> // If there is an error, break out of the command processing loop and return the error message.
                                match messagequeue with
                                | Some msgq -> msgq.Enqueue(e.Message)
                                | None -> ()
                                return Error e.Message
                        | Stop -> // Stops processing commands.
                            return StopState
                    }
                loop Continue
                |> Async.Ignore
            )
            ,
            cancelcommandqueue)

    let posttestcommand (cmd : TestCommand) msgsuccess args = // Posts a command to a particular test.
        let testname = extractarg ["-t";"--test"]  (id>>Some) args
        let testindex = 
            match testname with
            | Some tname ->
                let ti = ref 0
                if testnames.TryGetValue(tname,ti) then
                    Some ti.Value
                else
                    extractarg ["-I";"--index"]  ExtractInt args
            | None -> 
                extractarg ["-I";"--index"]  ExtractInt args
        let msgq = ref (messagequeue,testcommandqueue)
        let msg = ref ""
        match testindex with
        | None -> ()
        | Some index -> 
            if tests.TryGetValue(index,(msgq)) then
                (snd msgq.Value).Post(cmd)
                printbn "%s" (msgsuccess index)
            else
                if tests.Count > 0 then
                    printbn "Invalid test index. Valid indexes are 0 through %i" (tests.Count-1)
                else
                    printbn "Invalid test index. There are no tests."
    
    let postandreplytestcommand (cmd : AsyncReplyChannel<_> -> TestCommand) msgsuccess args = // Posts a command to a particular test and waits for a reply.
        let testname = extractarg ["-t";"--test"]  (id>>Some) args
        let testindex = 
            match testname with
            | Some tname ->
                let ti = ref 0
                if testnames.TryGetValue(tname,ti) then
                    Some ti.Value
                else
                    extractarg ["-I";"--index"]  ExtractInt args
            | None -> 
                extractarg ["-I";"--index"]  ExtractInt args
        let msgq = ref (messagequeue,testcommandqueue)
        let msg = ref ""
        match testindex with 
        | None -> None
        | Some index -> 
            if tests.TryGetValue(index,(msgq)) then
                let response =
                    (snd msgq.Value).PostAndReply(cmd)
                printbn "%s" (msgsuccess index)
                Some response
            else
                if tests.Count > 0 then
                    printbn "Invalid test index. Valid indexes are 0 through %i" (tests.Count-1)
                else
                    printbn "Invalid test index. There are no tests."
                None

    // A list of commands accessible from the program, along with information usable by the help command.
    // TODO: Finish moving all commands from the "loop" function to this list.
    let commandlist : (string list * (string list * ParamType * string) list * string * (_ -> _)) list ref = ref []
    commandlist :=
        [
            (
                ["teststep"; "tstep"], // List of aliases. The command may be called by any of these.
                [(["--test";"-t"],PtString,"The name of the test to send the command to."); // (List of argument aliases, argument type, description of the argument)
                (["--index";"-i"],PtInt,"The id of the test to send the command to.");
                ],
                "Pauses a test and has it run one step of processing.", // Description of the command.
                fun (pops,structures,args) -> // The function that executes the command. The parameters: (list of populations, list of structures, list of given command arguments)
                    posttestcommand TestStep (Printf.sprintf "Test %i successfully continued.") args
                    Some(pops, structures) // Return the new population list and structure list for the next command loop step. If none, break the loop and end the program.
            );
            (
                ["formattest"; "ftest"],
                [(["--format";"-f"],PtString,"This is a description of a parameter.");
                (["--test";"-t"],PtInt,"The id of the test to send the command to.");
                ],
                "This is a multi-line description of this format test. It checks if text formatting works as intended.\nThis should be on a newline.",
                fun (pops,structures,args) ->
                    Some(pops, structures)
            );
            (
                ["invalidatewindow"; "reformatwindow"; "rform"],
                [],
                "Invalidates the window view and reapplies text formatting.",
                fun (pops,structures,args) ->
                    Some(pops, structures)
            );
            (
                ["help"],
                [(["--name";"-n"],PtString,"The name of the command to get info of.");
                (["--verbosity";"-v"],PtInt,"How verbose the information should be. Valid verbosity levels: \n0:Simple. For general help, it displays command names only. For command specific help, it displays argument names and types.\n1:Normal. For general help, it displays command names, argument names, and argument types. For command specific help, it display argument names, argument types, argument descriptions, and the command description.");
                ],
                "Gets a list of commands, or if a command name is provided then it displays info about that command.",
                fun (pops,structures,args) ->
                    let commandname = extractarg ["--name";"-n"] (id>>Some) args
                    let verbositylevel = extractarg ["--verbosity";"-v"] (ExtractInt) args |> Option.defaultValue 1
                    match commandname with
                    | Some cname ->
                        let commandinfo =
                            List.tryFind (fun (nl,_,_,_) -> match nl with | head :: _ -> head = cname | [] -> false) commandlist.Value
                        match commandinfo with
                        | Some(cinamelist,ciparamlist,cidesc,_) ->
                            let rec pttostring ciparamtype =
                                match ciparamtype with 
                                | PtFloat -> "float"
                                | PtInt -> "int"
                                | PtString -> "string"
                                | PtUnit -> "unit"
                                | PtList cpt -> (pttostring cpt) + " list"
                            printb " "
                            for cn in cinamelist do
                                printb " %s" cn
                            printbnf 0 ""
                            if verbositylevel >= 1 then printbnf 4 "%s" cidesc
                            for (ciparamnamelist,ciparamtype,ciparamdesc) in ciparamlist do
                                printb " "
                                for pn in ciparamnamelist do
                                    printb " %s" pn
                                printbnf 4 ""
                                printbnf 4 "%s" (pttostring ciparamtype)
                                if verbositylevel >= 1 then printbnf 4 "%s" ciparamdesc
                        | _ -> printbn "Could not find command: %s" cname
                    | _ ->
                        for (cnamelist,cparams,_,_) in commandlist.Value do
                            printb " "
                            for cn in cnamelist do
                                printb " %s" cn
                            printbn ""
                            if verbositylevel >= 1 then 
                                for (cparamnamelist,_,_) in cparams do
                                    printb "   "
                                    for pn in cparamnamelist do
                                        printb " %s" pn
                                    printbn ""
                    Some(pops,structures)
            );
            (
                ["test"; "-test"],
                [(["--population";"-p"],PtInt,"The neural population id to include. May be used multiple times.");
                (["--structure";"-s"],PtInt,"The structure id to include. May be used multiple times.");
                (["--function";"-f"],PtString,"The name of the function to use as input.");
                (["--iterations";"-i"],PtInt,"Sets a limit on the number of iterations to process.");
                (["--samplecount";"-c"],PtInt,"How many iterations to include when sampling test results.");
                (["--randomseed";"-r"],PtInt,"The seed to use for the random number generator.");
                (["--silent";"-S"],PtInt,"Disables terminal log.");
                (["--logfile";"-l"],PtUnit,"The neural population id.");
                (["--loadtest";"-t"],PtString,"The file to load a saved test from.");
                (["--loadsoevnn";"-n"],PtString,"The file to load a saved soevnn from.");
                (["--pause";"-P"],PtUnit,"Does not run test upon creation.");
                ],
                "Creates a test. Each test runs simultaneously in the background. They may be sent commands and queried for information.",
                fun (pops,structures,args) ->
                    let populationindex = 
                        extractargs ["-p";"--population"] ExtractInt args |> (function | [] -> [1] | pl -> pl)
                        |> SomeIfForAll (IndexInRange pops)
                    let structureindex = 
                        extractargs ["-s";"--structure"] ExtractInt args |> (function | [] -> [1] | pl -> pl)
                        |> SomeIfForAll (IndexInRange structures)
                    let functionname = extractarg ["-f";"--function"]  (id >> Some) args |> defaultoption "constant"
                    let iterations = extractarg ["-i";"--iterations"]  ExtractInt args
                    let samples = extractarg ["-c";"--samplecount"]  ExtractInt args |> defaultoption 100
                    let seed = extractarg ["-r";"--randomseed"]  ExtractInt  args
                    let silent = List.exists (IsAny ["-S"; "--silent";"-q";"--queue"]) args
                    let logfile = extractarg ["-l"; "--logfile"] ExtractFile args
                    let shouldqueue = true
                    let loadtest = extractarg ["-t" ; "--loadtest"] (id >> Some) args
                    let loadsoevnn = extractarg ["-n"; "--loadsoevnn"] (id >> Some) args
                    let startpaused = shouldqueue && List.exists (IsAny ["-P"; "--pause"]) args

                    let testcommandqueue = 
                        let somequeue = [loadtest; loadsoevnn]
                        if List.exists (Option.isSome) somequeue then
                            let mp = new MailboxProcessor<TestCommand>(fun mp -> async { () })
                            if startpaused then mp.Post(TestPause)
                            match loadtest with | None -> () | Some value -> mp.Post(TestLoadTest(LoadFromFileName(value)))  
                            match loadsoevnn with | None -> () | Some value -> mp.Post(TestLoadSoevnn(LoadFromFileName(value)))
                            
                            Some mp
                        else
                            None
                    
                    let rec getpops (result : Map<int,int>) (pil : int list) =
                        match pil with
                        | [] -> 
                            result
                            |> Map.toList
                            |> List.map (fun (index,cnt) -> (cnt,index))
                        | pi :: remsil ->
                            getpops (result.Add(pi,if result.ContainsKey(pi) then result.[pi] + 1 else 1)) remsil
                    let rec getns (result : Map<int,int>) (sil : int list) =
                        match sil with
                        | [] -> 
                            result
                            |> Map.toList
                            |> List.collect (fun (index,cnt) -> [for _ in [1..cnt] do yield structures.[index]])
                        | si :: remsil ->
                            getns (result.Add(si,if result.ContainsKey(si) then result.[si] + 1 else 1)) remsil
                    let ns =
                        structureindex
                        |> Option.map (getns Map.empty)
                        |> Option.map (fun sil -> match populationindex with | Some([]) -> sil | None -> sil | Some(pl) -> sil @ [getpops Map.empty pl])
                    
                    match logfile with
                    | Some writer -> writer.Dispose()
                    | None -> ()
                    commandqueue.Post(Test(pops,(match ns with | Some value -> value | None -> []), seed, iterations, samples, (functionname, inputfunctions.[functionname]), logfile, silent, (Some messagequeue), testcommandqueue) )
                    
                    Some(pops, structures)
            );
            (
                ["quit";"exit"],
                [],
                "Quits the program.",
                fun (pops,structures,args) ->
                    None
            );
            (
                ["addstructure"; "addstruct"],
                [(["--population"; "-p"],PtInt,"The index of the neural population to include.");
                (["--populations"; "-P"],PtList PtInt,"The indices of the neural population to include.");
                ],
                "Adds a structure made up of populations of neurons.",
                fun (pops,structures,args) ->
                    let populationindex = extractargs ["-p";"--population"] ExtractInt args |> (function | [] -> [1] | pl -> pl)
                    let populationindex =  
                        extractarg ["-P";"--populations"] (ExtractArray ExtractInt) args |> defaultoption []
                        |> List.append populationindex
                    let rec getns (result : Map<int,int>) (sil : int list) =
                        match sil with
                        | [] -> 
                            result
                            |> Map.toList
                            |> List.map (fun (a,b) -> (b,a))
                        | si :: remsil ->
                            getns (result.Add(si,if result.ContainsKey(si) then result.[si] + 1 else 1)) remsil
                    if List.forall (fun (i:int) -> i >= 0 && i < pops.Length) populationindex then
                        Some(pops, (structures @ [getns Map.empty populationindex]))
                    else
                        Some(pops,structures)
            );
            (
                ["addpopulation";"addpop"],
                [(["Type intra-inter balance"], PtFloat, "Valid Range: 0.0 - 1.0 \n" + "The balance of synaptic connections to other neurons of the same type vs those of a different type. 0 is the former, 1 is the latter.");
                (["Structure intra-inter balance"], PtFloat, "Valid Range: 0.0 - 1.0 \n" + "The balance of synaptic connections to other neurons in the same structure vs those of a different structure. 0 is the former, 1 is the latter.");
                (["Cluster intra-inter balance"], PtFloat, "Valid Range: 0.0 - 1.0 \n" + "The balance of synaptic connections to other neurons in the same cluster vs those of a different cluster. 0 is the former, 1 is the latter.");
                (["Synapse direct-inverse balance"], PtFloat, "Valid Range: 0.0 - 1.0 \n" + "The balance of direct synaptic connections vs inverse connections. 0 is the former, 1 is the latter.");
                (["Clustersize"], PtInt, "Valid Range: 1 and higher \n" + "How many neurons per cluster.");
                (["Current accumulation rate"], PtFloat, "Valid Range: 0.0 - 1.0 \n" + "The rate of short-term desensitization. Acts as working memory. ");
                (["Expected accumulation rate"], PtFloat, "Valid Range: 0.0 - 1.0 \n" + "The rate of long-term desensitization. Should be lower than the current accumulation rate. Acts as long term memory.");
                (["Adaption rate"], PtFloat, "Valid Range: 0.0 - 1.0 \n" + "The rate at which synaptic connections are formed/broken per neuron. Acts as long term memory. Additionally, it optimizes the information compression.");
                (["Minimum synapses per neuron."], PtInt, "Valid Range: 1 and higher \n" + "The number of synapses per neuron affects the minimum amount the neuron generalizes information.");
                (["Maximum synapses per neuron."], PtInt, "Valid Range: The minimum and higher \n" + "The number of synapses per neuron affects the maximum amount the neuron generalizes information.");
                (["Pathway type."], PtInt, "Valid Range: 0 - 3 \n" + "The way in which neural pathways are formed. These pathways are routes through which information travels though with minimal delay. The types are:\n"
                 + "0 - Cluster: The path this neuron is a part of is formed from it's cluster.\n"
                 + "1 - Group: The path this neuron is a part of is formed from it's type within a structure.\n"
                 + "2 - Structure: The path this neuron is a part of is formed from it's structure.\n"
                 + "3 - Type: The path this neuron is a part of is formed from it's type across structures.");
                ],
                ("Adds a population of neurons of a single neural type. Note, uses non-standard argument input. The arguments are unnamed and always in the same order. They are in the format of:\n"
                + "[addpopulation/addpop] float float float float int float float float int int int\n"
                + "Example: addpopulation 0.4 0.2 0.5 0.1 10 0.9 0.05 0.02 10 20 1"),
                fun (pops,structures,args) ->
                    match ExtractPopulation ExtractFloat ExtractInt args with
                    | Some ((intrainterbalancegroup, intrainterbalancestructure, intrainterbalancecluster, directinversebalance, clustersize, currentaccumrate, expectedaccumrate, adaptionrate, synapsecountmin, synapsecountmax, pathway) as value) -> 
                        printbn "Added new population:"
                        printbnf 2 "Clustersize: %i" clustersize
                        printbnf 2 "Currentaccumrate: %f" currentaccumrate
                        printbnf 2 "Expectedaccumrate: %f" expectedaccumrate
                        printbnf 2 "Adaptionrate: %f" adaptionrate
                        printbnf 2 "Synapsecountmin: %i" synapsecountmin
                        printbnf 2 "Synapsecountmax: %i" synapsecountmax
                        printbnf 2 "PathwayType: %s" (string(getpathway pathway))
                        Some((pops @ [value]), structures)
                    | None -> 
                        printbn "Bad Arguments"
                        for s in args do printbn "%s" (s)
                        //for s in argl do Console.WriteLine(s)
                        Some(pops, structures)
            );
            (
                ["lpop";"listpopulations"],
                [(["-v";"--verbose"], PtInt, "Creates a fully labeled layout of the information." );
                ],
                ("Lists the populations of neurons of each neural type. Lists the parameters in the same order as entered."),
                fun (pops,structures,args) ->
                    let isverbose = List.exists (IsAny ["-v";"--verbose"]) args 
                    if isverbose then
                        for i in [0 .. pops.Length - 1] do
                            let intrainterbalancegroup, intrainterbalancestructure, intrainterbalancecluster, directinversebalance, clustersize, currentaccumrate, expectedaccumrate, adaptionrate, synapsecountmin, synapsecountmax, pathway = pops.[i]
                            printbn "Index %i:" i 
                            printbn "  IntraInterBalanceGroup: %f" intrainterbalancegroup
                            printbn "  IntraInterBalanceStructure: %f" intrainterbalancestructure
                            printbn "  IntraInterBalanceCluster: %f" intrainterbalancecluster
                            printbn "  DirectInverseBalance: %f" directinversebalance
                            printbn "  ClusterSize: %i" clustersize
                            printbn "  CurrentAccumRate: %f" currentaccumrate
                            printbn "  ExpectedAccumRate: %f" expectedaccumrate
                            printbn "  AdaptionRate: %f" adaptionrate
                            printbn "  SynapseCountMin: %i" synapsecountmin
                            printbn "  SynapseCountMax: %i" synapsecountmax
                            printbn "  PathwayType: %s" (string(getpathway pathway))
                    else
                        for i in [0 .. pops.Length - 1] do
                            let intrainterbalancegroup, intrainterbalancestructure, intrainterbalancecluster, directinversebalance, clustersize, currentaccumrate, expectedaccumrate, adaptionrate, synapsecountmin, synapsecountmax, pathway = pops.[i]
                            printbn "%i: %f, %f, %f, %f, %i, %f, %f, %f, %i, %i, %s" i intrainterbalancegroup intrainterbalancestructure intrainterbalancecluster directinversebalance clustersize currentaccumrate expectedaccumrate adaptionrate synapsecountmin synapsecountmax (string(getpathway pathway))
                    Some(pops, structures)
            );
            (
                ["lstruct";"liststructures"],
                [(["-v";"--verbose"], PtUnit, "Creates a fully labeled layout of the information." );
                ],
                ("Lists the populations of neurons of each neural type. Lists the parameters in the same order as entered."),
                fun (pops,structures,args) ->
                    let isverbose = List.exists (IsAny ["-v";"--verbose"]) args 
                    if isverbose then
                        for i in [0 .. structures.Length - 1] do
                            let s = structures.[i]
                            printbn "Index %i:" i
                            printbf 4 "Pop Id"; printbnf 12 "|  Count"
                            printbnf 2 "----------|----------"
                            for pop in s do
                                printbf 4 "%i" (snd pop); printbnf 12 "|  %i" (fst pop)

                    else
                        for i in [0 .. structures.Length - 1] do
                            let s = structures.[i]
                            printb "%i: " i
                            for pop in s do
                                printb "(%i, %i); " (snd pop) (fst pop)
                            printbn ""
                    Some(pops, structures)
            );
            (
                ["listtestfiles"; "ltfiles"],
                [
                ],
                "Lists the test files in this directory.",
                fun (pops,structures,args) ->
                    printbn "Soevnn Test Files:"
                    for (name) in (Directory.EnumerateFiles("./","*.soevnntest")) do
                        printbn "%s" name
                    Some(pops,structures)
            );
            (
                ["listsoevnnfiles"; "lsfiles"],
                [
                ],
                "Lists the test files in this directory.",
                fun (pops,structures,args) ->
                    printbn "Soevnn Test Files:"
                    for (name) in (Directory.EnumerateFiles("./","*.soevnn")) do
                        printbn "%s" name
                    Some(pops,structures)
            );
            (
                ["testpause"; "tpause"],
                [(["-I";"--index"], PtInt, "Creates a fully labeled layout of the information." )
                ],
                "Lists the test files in this directory.",
                fun (pops,structures,args) ->
                    posttestcommand TestPause (Printf.sprintf "Test %i successfully paused.") args
                    Some(pops,structures)
            );
        ]
        |> List.sortBy (fun (names : string list, _, _, _) -> names.Head)

    commandqueue.Start() // Initiates the mailbox processor for commands. 
    let rec loop (prevwindowwidth) (pops : (float * float * float * float * int * float * float * float * int * int * int) list) (structures : (int * int) list list) argl = // The main program loop.
        let indexlist l il = // Unused. Unsure what it was for. TODO: Remove indexlist.
            il
            |> SomeIfForAll (IndexInRange l)
            |> Option.map (fun il -> List.map (fun i -> l.[i]) il)
        let currentwindowwidth = Console.WindowWidth
        let loop = loop currentwindowwidth
        if prevwindowwidth <> currentwindowwidth then // If the window width has changed then...
            Console.Clear()  
            Console.Write(buffer.GetFormattedString(Console.WindowWidth)) // ...redraw contents formatted for the new width.
        let readline () = // Reads a line from the console. This has to be handled specially for the formatted print buffer.
            let input = Console.ReadLine()
            readbn "%s" input // Updates the buffer with the line read.
            input
        match argl with
        | Command(["-break"]) args ->    
            ()
            loop pops structures (readline() |> getargs)
        | Commands commandlist.Value (args, f) -> 
            match f(pops,structures,args) with
            | Some (ps,ss) -> loop ps ss (readline() |> getargs)
            | None -> ()
        | "messages" :: args -> 
            let testindex = extractarg ["-t";"--test"]  ExtractInt args
            let msgq = ref (messagequeue,testcommandqueue)
            let msg = ref ""
            match testindex with 
            | None -> 
                while (fst msgq.Value).TryDequeue(msg) do
                    printbn "%s" msg.Value
            | Some index -> 
                if tests.TryGetValue(index,(msgq)) then
                    while (fst msgq.Value).TryDequeue(msg) do
                        printbn "%s" msg.Value
                else
                    if tests.Count > 0 then
                        printbn "Invalid test index. Valid indexes are 0 through %i" (tests.Count-1)
                    else
                        printbn "Invalid test index. There are no tests."
            
            loop pops structures (readline() |> getargs)
        | Command ["ltnames"; "listtestnames"] args ->
            for (name,index) in (Seq.zip testnames.Keys testnames.Values) do
                printbn "%s: %i" name index
            loop pops structures (readline() |> getargs)
        | Command ["addtestname"; "addtname"] args ->
            let mname = extractarg ["-n";"--name"]  (id>>Some) args
            let mindex = extractarg ["-I";"--index"]  ExtractInt args
            match (mname,mindex) with
            | (Some name, Some index) -> 
                if index >= 0 && index < tests.Count then
                    if testnames.TryAdd(name,index) then
                        printbn "Added name \"%s\" for test %i" name index
                    else
                        printbn "Failed to add name."
            | _ ->
                printbn "Failed to add name."
            loop pops structures (readline() |> getargs)
        | Command ["testcontinue"; "tcont"] args -> 
            posttestcommand TestContinue (Printf.sprintf "Test %i successfully continued.") args
            loop pops structures (readline() |> getargs)
        | Command ["testsetfunction"; "tfunc"] args -> 
            let functionname = extractarg ["-f";"--function"]  (id >> Some) args |> defaultoption "constant"
            let newfunc = if inputfunctions.ContainsKey(functionname) then inputfunctions.[functionname] else inputfunctions.["constant"]
            posttestcommand (TestSetFunction(functionname,newfunc)) (Printf.sprintf "Test %i input function updated successfully.") args
            loop pops structures (readline() |> getargs)
        | Command ["testsetremainingiterations"; "tsriter"] args -> 
            let iterations = extractarg ["-i";"--iterations"]  ExtractInt args
            posttestcommand (TestSetRemainingIterations(iterations)) (Printf.sprintf "Test %i successfully continued.") args
            loop pops structures (readline() |> getargs)

        | Command ["testgetprogress"; "tprog"] args -> 
            
            match postandreplytestcommand (fun rc -> TestGetProgress(rc)) (Printf.sprintf "Test %i successfully returned progress.") args with
            | Some((iteration,currentduration),Some(maxiteration,estimatedremainingtime)) ->
                printbn "%f%% Training Complete / %s Time Lapsed / %s Estimated Time Remaining" (float(1000 * iteration / maxiteration) * 0.1) (timespantostring currentduration) (timespantostring estimatedremainingtime)
            | Some((iteration,currentduration),None) ->
                printbn "Iteration %i / %s Time Lapsed" iteration (timespantostring currentduration) 
            | None ->
                ()
            loop pops structures (readline() |> getargs)
        
        | Command ["testgetsamples"; "tsamp"] args -> 
            let logfile = extractarg ["-l"; "--logfile"] ExtractFile args // |> Option.map (fun (sw : StreamWriter) -> TextWriter(sw))
            

            match postandreplytestcommand (fun rc -> TestGetSamples(rc)) (Printf.sprintf "Test %i successfully returned samples.") args with
            | Some(sfunction,smimic,serror,sabsoluteerror) ->
                match logfile with 
                | None ->
                    printsamples None sfunction "Input Function Stats"
                    printsamples None smimic "Mimic Function Stats"
                    printsamples None serror "Error Stats"
                    printsamples None sabsoluteerror "Absolute Error Stats"
                | Some log ->
                    logsamples log None sfunction "Input Function Stats"
                    logsamples log None smimic "Mimic Function Stats"
                    logsamples log None serror "Error Stats"
                    logsamples log None sabsoluteerror "Absolute Error Stats"
                    log.Dispose()
            | None ->
                ()
            loop pops structures (readline() |> getargs)
        | Command(["saveloadnervoussystemstest" ; "slnstest"]) args ->
            TestSaveLoadNervousSystem ()
            loop pops structures (readline() |> getargs)
        | "testcount" :: _ -> 
            let cnt = tests.Count
            if cnt > 0 then
                printbn "There are %i tests. Valid test indexes are 0 through %i" cnt (cnt-1)
            else
                printbn "There are no tests."
            loop pops structures (readline() |> getargs)
        | Command(["testgetsamplecount"; "tgscount"]) args ->
            match postandreplytestcommand (fun rc -> TestGetSampleCount(rc)) (Printf.sprintf "Test %i successfully returned sample count.") args with
            | Some(samplecount) ->
                printbn "Up to %i samples are gathered." samplecount
            | None ->
                ()
            loop pops structures (readline() |> getargs)
        | Command(["testgetfunction"; "tgfunc"]) args ->
            match postandreplytestcommand (fun rc -> TestGetFunction(rc)) (Printf.sprintf "Test %i successfully returned sample count.") args with
            | Some(fname,fvalue) ->
                printbn "The current function is \"%s\"" fname
            | None ->
                ()
            loop pops structures (readline() |> getargs)
        | Command(["testgetremainingiterations"; "tgriter"]) args ->
            match postandreplytestcommand (fun rc -> TestGetRemainingIterations(rc)) (Printf.sprintf "Test %i successfully returned sample count.") args with
            | Some(Some(remiter)) ->
                printbn "There are %i remaining iterations." remiter
            | Some(None) ->
                printbn "There is not a set number of iterations. Testing will continue indefinitely."
            | None ->
                ()
            loop pops structures (readline() |> getargs)
        | "--interactive" :: _ -> loop pops structures (readline() |> getargs) 
        | _ -> 
            printbn "Bad Command:"
            for s in argl do printbn "%s" (s.ToString())
            loop pops structures (readline() |> getargs)
        

    loop Console.WindowWidth populations initialstructures (List.ofArray argv) // Start the program loop.

    0 // return an integer exit code
