module Soevnn.Console.Testing


open System
open Soevnn.Core
open Soevnn.Parallel.Core
open System.IO
open Soevnn.Serialization
open Microsoft.FSharp.Reflection
open Soevnn.Console.Utilities


let getpathway i =
    if i = 0 then
        NpCluster
    else if i = 1 then
        NpGroup
    else if i = 2 then
        NpStructure
    else if i = 3 then
        NpType
    else
        NpCluster

let inputfunctions =
    Map.ofList
        [
            ("constant", (fun _ -> 100.0));
            ("sin", (fun i -> 100.0 * sin (0.1 * float i)));
            ("mod", (fun i -> 100.0 * Math.IEEERemainder(0.1 * float i, 1.0)))
        ]




let TestSaveLoadNervousSystem () =
    let ns = CreateNervousSystemFromStructures [[(3, NeuralType(0.1,0.2,0.3,0.4,5,6,7,0.8,0.9,0.01,2,NtOffset,NpCluster,0.11,[]))]] [] []
    let neurons = CreateIndexedNeuronsParallel ns

    use memstream = new MemoryStream()
    try
        SaveNervousSystem ns neurons memstream
    with
        | exn -> printbn "Save error:   %s" exn.Message
    memstream.Position <- int64 0
    use binaryreader = new BinaryReader(memstream)
    
    let result =
            let slns = LoadNervousSystem(memstream)
            match slns with
            | Some(loadedns,loadedneurons) -> 
                let areneuronsequal (a : Neuron) (b : Neuron) =
                    a.Axon = b.Axon &&
                    a.Dendrites = b.Dendrites &&
                    a.NeuralType = b.NeuralType &&
                    (Array.forall2 (=) (a.MessageQueue.ToArray()) (b.MessageQueue.ToArray())) &&
                    a.NeuralType = b.NeuralType &&
                    a.ExpectedAccum = b.ExpectedAccum &&
                    a.CurrentAccum = b.CurrentAccum &&
                    a.Variance = b.Variance
                
                let nsequal = loadedns = ns
                let neuronsequal = neurons.Length = loadedneurons.Length && Seq.forall2 (fun (a,b) (c,d) -> a= c && areneuronsequal b d) neurons loadedneurons
                match (nsequal,neuronsequal) with
                | (true,true) -> None
                | (true,false) -> failwith ("Neurons not equal. Unequal neurons: " + (Seq.zip neurons loadedneurons |> Seq.filter (fun (a,b) -> a <> b) |> Seq.mapi (fun i (a,b) -> string i + ", ") |> Seq.fold (fun state item -> state+item) "")); Some("Neurons not equal.")
                | (false,true) -> Some("NervousSystem not equal.")
                | (false,false) -> Some("Neither the Neurons nor the NervousSystem are equal.")
            | None -> Some("Load failed.")
    
    match result with
    | None -> printbn "O - Passed Test: SaveLoadNeuralTypes"
    | Some(msg) -> printbn "O - Failed Test: SaveLoadNeuralTypes"; printbn "%s" msg
    






type Stats(data : float seq) =
    member this.Minimum = Seq.min data
    member this.Maximum = Seq.max data
    member this.Average = Seq.average data
    member this.Median = 
        let ordereddata = Seq.sort data
        Seq.item (Seq.length ordereddata / 2) ordereddata
    member this.Variance = Seq.averageBy (fun d -> pown (d - this.Average) 2) data |> Math.Sqrt
    member this.Range = this.Maximum - this.Minimum

type LoadFrom =
| LoadFromFileName of string
| LoadFromStream of Stream

type SaveTo =
| SaveToFileName of string
| SaveToBinaryWriter of BinaryWriter

type TestCommand =
| TestPause
| TestContinue
| TestStep
| TestSaveTest of string
| TestSaveSoevnn of string
| TestSetRemainingIterations of int option
| TestGetRemainingIterations of int option AsyncReplyChannel
| TestSetSampleCount of int
| TestGetSampleCount of int AsyncReplyChannel
| TestGetSamples of (Stats * Stats * Stats * Stats) AsyncReplyChannel
| TestGetProgress of ((int * TimeSpan) * (int * TimeSpan) option) AsyncReplyChannel // (CurrentIterations, TimeLapsed), (TotalIterations, EstimatedTimeRemaining)
| TestLoadTest of LoadFrom
| TestLoadSoevnn of LoadFrom
| TestSetFunction of string * (int -> float)
| TestGetFunction of (string * (int -> float)) AsyncReplyChannel


let ExtractFile (value:string) : _ option =
    try
        if not (value.Contains('/')) then
            File.AppendText(@"./"+value)
            |> Some
        else
            File.AppendText(value)
            |> Some
    with
        | _ -> None


let SaveFile (filetype : string) (value : string) : _ option =
    try
        let path =
            if not (value.Contains('/')) then
                @"./"+value
            else
                value
            + if value.Contains('.') then "" else ("." + filetype)
        File.Open(path,FileMode.Create,FileAccess.ReadWrite)
        |> Some 
    with
        | _ -> None

let LoadFile (filetype : string) (value:string) : _ option =
    try
        
        let path =
            if not (value.Contains('/')) then
                @"./"+value
            else
                value
            + if value.Contains('.') then "" else "." + filetype
            
        File.OpenRead(path)
        |> Some
    with
        | _ -> None

let printsamples (samples : float [] option) (samplestats : Stats) (label : string) =
    printbn "%s" label
    if samples.IsSome then printbn "%A" samples.Value
    printbn "Minimum output: %f" samplestats.Minimum
    printbn "Maximum output: %f" samplestats.Maximum
    printbn "Range of output: %f" samplestats.Range
    printbn "Median output: %f" samplestats.Median
    printbn "Average output: %f" samplestats.Average
    printbn "Variance of output: %f" samplestats.Variance
    
let logsamples (writer) (samples : float [] option) (samplesstats : Stats) (label : string) =
    fprintfn writer "%s" label
    if samples.IsSome then fprintfn writer "%A" samples.Value
    fprintfn writer "Minimum output: %f" samplesstats.Minimum
    fprintfn writer "Maximum output: %f" samplesstats.Maximum
    fprintfn writer "Range of output: %f" samplesstats.Range
    fprintfn writer "Median output: %f" samplesstats.Median
    fprintfn writer "Average output: %f" samplesstats.Average
    fprintfn writer "Variance of output: %f" samplesstats.Variance

let TestNervousSystem neuraltypes neuralstructures seed (trainingsteps : int option) (testingsteps : int) (fname, inputfunction) (logfile: StreamWriter option) silent (messagequeue : System.Collections.Concurrent.ConcurrentQueue<string> option) (commandqueue : MailboxProcessor<TestCommand> option) = 
    let initialdatetime = DateTime.UtcNow
    if not silent then
        printbn "Start Time: %s" <| DateTime.Now.ToShortTimeString()
    
    
    let input = NeuralAddress(0,0,0,0)
    let error = NeuralAddress(0,0,0,2)
    let mimic = NeuralAddress(0,0,0,1)
    let senses = [input;error]
    let rand = match seed with | Some(seedvalue) -> System.Random(seedvalue) | None -> System.Random()
    
    


    let nt = 
        List.map 
            (fun (intrainterbalancegroup, intrainterbalancestructure, intrainterbalancecluster, directinversebalance, clustersize, currentaccumrate, expectedaccumrate, adaptionrate, synapsecountmin, synapsecountmax, pathwaytype) -> 
                (NeuralType(intrainterbalancegroup,intrainterbalancestructure,intrainterbalancecluster, directinversebalance,synapsecountmin, synapsecountmax,synapsecountmax,adaptionrate,currentaccumrate,expectedaccumrate,clustersize,NtOffset,getpathway pathwaytype,0.25,[SensoryAdaption(input, 2.0, 1.0); SensoryAdaption(error, 0.5, 0.25)])))
            neuraltypes
    let nsl =
        neuralstructures
        |> List.choose
            (fun ns ->
                ns
                |> SomeIfForAll (snd >> IndexInRange nt)
                |> Option.map (List.map (fun (cnt,index) -> (cnt,nt.[index])))
            )

    let ns = CreateNervousSystemFromStructures nsl senses [mimic]
    let cmap = CreateConnectivityMap ns
    let indexedneurons = (CreateIndexedNeuronsParallel ns)
    let smap = CreateSensoryMapParallel ns (indexedneurons) senses 
    let initialneurons= 
        CreateConnectionsParallel rand ns indexedneurons (cmap) (smap) 
    let initialaddresses =
        initialneurons |> Array.mapi (fun i (k,v) -> (k,i)) |> Map.ofArray
    let processstep iteration addresses neurons newneurons smap threads currentfunction  : ((NeuralAddress * Neuron) []) * SensoryMapParallel * float [] =
        (
            async { return (ProcessAllParallel ns cmap smap rand addresses neurons newneurons threads (Map.ofList [(input, inputfunction iteration);(error,(snd neurons.[addresses.[mimic]]).Axon - currentfunction (iteration - 1))]) (Map.ofList [(mimic,0)]))}
            ,
            async { return (CreateSensoryMapParallel ns neurons senses)}
        ) 
        |> (fun (procall,createsmap) -> 
            let taskprocall = Async.StartAsTask procall
            let taskcreatesmap = Async.StartAsTask createsmap
            (
                taskprocall.GetAwaiter().GetResult(),
                taskcreatesmap.GetAwaiter().GetResult()
            )
        )
        |> (fun ((a,b),c) -> 
            (a,c,Array.concat [[|(inputfunction iteration)|];b]))

    let rec processsteps (iteration : int) (maxiteration : int Option) previousdatetime addresses (nervoussystem : NervousSystem) (neurons : (NeuralAddress*Neuron)[]) (newneurons : (NeuralAddress*Neuron)[]) smap threads currentfunction samplecount (samples : System.Collections.Generic.Queue<float []>) ispaused (previousduration : TimeSpan) (sessionstarttime : DateTime) : ((NeuralAddress * Neuron) []) * SensoryMapParallel * float [] [] =
        let currentdatetime = if ispaused then sessionstarttime else DateTime.UtcNow
        let currenttimelapsed = TimeSpan.FromTicks(currentdatetime.Subtract(sessionstarttime).Ticks + previousduration.Ticks)
        let setl = 
            match maxiteration with
            | Some maxiter ->
                TimeSpan.FromTicks(currenttimelapsed.Ticks * int64 (maxiter - iteration) / int64 iteration) |> Some
            | None -> None
        
        let etls =
            match setl with
            | Some etl ->
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
            | None -> "N/A"
        
        if maxiteration.IsSome && (100000 * iteration / maxiteration.Value) % 100  = 0 then
            if (not silent) && messagequeue.IsNone then
                printbn "%f%% Training Complete / %s Estimated Time Remaining" (float(1000 * iteration / maxiteration.Value) * 0.1) etls
            if messagequeue.IsSome then
                messagequeue.Value.Enqueue(sprintf "%f%% Training Complete / %s Estimated Time Remaining" (float(1000 * iteration / maxiteration.Value) * 0.1) etls)
        if (match maxiteration with | None -> true | Some maxiter when (iteration < maxiter) -> true | _ -> false) then
            let newneurons,newsensorymap,sample = 
                    if ispaused then
                        neurons,smap,[||]
                    else
                        processstep iteration addresses neurons newneurons smap threads (snd currentfunction)
            if not ispaused then
                samples.Enqueue(sample)
                if samples.Count > samplecount then samples.Dequeue() |> ignore
            let inline basicsteps () =
                processsteps 
                    (if ispaused then iteration else iteration + 1)
                    maxiteration
                    currentdatetime
                    addresses
                    nervoussystem
                    newneurons
                    neurons
                    newsensorymap
                    threads
                    currentfunction
                    samplecount
                    samples
                    ispaused
                    previousduration
                    (if ispaused then currentdatetime else sessionstarttime)
            if commandqueue.IsSome then
                let tmsg = 
                    if ispaused || (match maxiteration with | None -> false | Some maxiter when (iteration < maxiter) -> false | _ -> true) then
                        commandqueue.Value.Receive() |> Async.RunSynchronously |> Some
                    else
                        if commandqueue.Value.CurrentQueueLength > 0 then
                            commandqueue.Value.Receive() |> Async.RunSynchronously |> Some
                        else
                            None
                
                if tmsg.IsSome then
                    match tmsg.Value with
                    | TestContinue ->
                        processsteps 
                            (iteration + 1)
                            maxiteration
                            currentdatetime
                            addresses
                            nervoussystem
                            newneurons
                            neurons
                            newsensorymap
                            threads
                            currentfunction
                            samplecount
                            samples
                            false
                            previousduration
                            (if ispaused then currentdatetime else sessionstarttime)
                    | TestSetSampleCount newsamplecount ->
                        processsteps 
                            (if ispaused then iteration else iteration + 1)
                            maxiteration
                            currentdatetime
                            addresses
                            nervoussystem
                            newneurons
                            neurons
                            newsensorymap
                            threads
                            currentfunction
                            newsamplecount
                            samples
                            ispaused
                            previousduration
                            (if ispaused then currentdatetime else sessionstarttime)
                    | TestPause ->
                        processsteps 
                            iteration
                            maxiteration
                            currentdatetime
                            addresses
                            nervoussystem
                            newneurons
                            neurons
                            newsensorymap
                            threads
                            currentfunction
                            samplecount
                            samples
                            true
                            (previousduration+(currentdatetime-sessionstarttime))
                            (if ispaused then currentdatetime else sessionstarttime)
                    | TestStep ->
                        processsteps 
                            (iteration + 1)
                            (iteration + 1 |> Some)
                            currentdatetime
                            addresses
                            nervoussystem
                            newneurons
                            neurons
                            newsensorymap
                            threads
                            currentfunction
                            samplecount
                            samples
                            false
                            previousduration
                            (if ispaused then currentdatetime else sessionstarttime)
                    | TestSaveSoevnn soevnnsavefilename ->
                        let maybeasavefile = SaveFile "soevnn" soevnnsavefilename
                        match maybeasavefile with
                        | Some(savefile) ->
                            Soevnn.Serialization.SaveNervousSystem ns newneurons savefile
                            savefile.Flush()
                            savefile.Close()
                        | None -> ()
                        processsteps 
                            (if ispaused then iteration else iteration + 1)
                            maxiteration
                            currentdatetime
                            addresses
                            nervoussystem
                            newneurons
                            neurons
                            newsensorymap
                            threads
                            currentfunction
                            samplecount
                            samples
                            ispaused
                            previousduration
                            (if ispaused then currentdatetime else sessionstarttime)
                    | TestGetSamples results ->
                        let samplearrays = samples.ToArray()
                        let functionsamples = Array.map (fun (a:float[]) -> a.[0]) samplearrays
                        let mimicsamples = Array.map (fun (a:float[]) -> a.[1]) samplearrays
                        let errorsamples = Array.map (fun (a:float[]) -> a.[1] - a.[0]) samplearrays
                        let absoluteerrorsamples = Array.map (fun (a:float[]) -> (a.[1] - a.[0]) |> abs) samplearrays
                        results.Reply(Stats functionsamples,Stats mimicsamples, Stats errorsamples, Stats absoluteerrorsamples)
                        basicsteps()
                    | TestGetProgress results ->
                        match (maxiteration,setl) with
                        | (Some miter,Some etl) ->
                            results.Reply((iteration,currenttimelapsed+previousduration),Some(miter,etl))
                        | _ ->
                            results.Reply((iteration,currenttimelapsed+previousduration),None)
                        basicsteps()
                    | TestSetRemainingIterations newremainingiterations ->
                        processsteps 
                            (if ispaused then iteration else iteration + 1)
                            (match newremainingiterations with | Some v -> Some(iteration+v) | None -> None)
                            currentdatetime
                            addresses
                            nervoussystem
                            newneurons
                            neurons
                            newsensorymap
                            threads
                            currentfunction
                            samplecount
                            samples
                            ispaused
                            previousduration
                            (if ispaused then currentdatetime else sessionstarttime)
                    | TestSaveTest savename ->
                        let maybeasavefile = SaveFile "soevnntest" savename
                        match maybeasavefile with
                        | Some(savefile) ->
                            MessagePack.MessagePackSerializer.Serialize(savefile, SoevnnTestData(0,1,1,iteration,maxiteration,samplecount,samples,previousduration.Ticks+currentdatetime.Ticks-sessionstarttime.Ticks), MessagePackOptions)
                            Soevnn.Serialization.SaveNervousSystem ns newneurons savefile
                            savefile.Flush()
                            savefile.Close()
                        | None -> 
                            ()
                        basicsteps()
                      // Error: Does not load Soevnn correctly even though all save/load tests come back successful. See comment for TestLoadSoevnn.
                    | TestLoadTest loadfrom ->
                        //try
                            let maybealoadfile = 
                                match loadfrom with
                                | LoadFromFileName loadfilename ->
                                    LoadFile "soevnntest" loadfilename
                                    |> Option.map (fun fs -> fs :> Stream)
                                | LoadFromStream stream ->
                                    Some stream

                            match maybealoadfile with
                            | Some soevnnreader ->
                                let sr = soevnnreader
                                let testdata = 
                                    try
                                        Some <| MessagePack.MessagePackSerializer.Deserialize<SoevnnTestData>(sr, MessagePackOptions)
                                    with
                                        | :? MessagePack.MessagePackSerializationException -> None
                                match testdata with
                                | Some testdata ->
                                    match (testdata.MajorVersion,testdata.MinorVersion,testdata.RevisionNumber) with
                                    | (0,1,1) ->
                                        let newiteration = testdata.Iteration
                                        let newmaxiteration = testdata.MaxIteration
                                        let newsamplecount = testdata.SampleCount
                                        let newsamples =
                                            testdata.Samples
                                        let newpreviousduration = TimeSpan.FromTicks(testdata.UpTime)
                                        let newns = Soevnn.Serialization.LoadNervousSystem sr
                                        sr.Dispose()
                                        match newns with
                                        | Some (nns,nn) ->
                                            let nna = nn
                                            let newnna = Array.map (fun (neuraladdress,neuron : Neuron) -> (neuraladdress,new Neuron(neuron.NeuralType,neuron.Dendrites,neuron.Axon,neuron.CurrentAccumRate, neuron.ExpectedAccumRate, neuron.CurrentAccum, neuron.ExpectedAccum, neuron.Variance))) nna
                                            processsteps
                                                newiteration
                                                newmaxiteration
                                                currentdatetime
                                                (Array.mapi (fun i (a,_) -> (a,i)) nn |> Map.ofArray)
                                                nns
                                                nna
                                                newnna
                                                (CreateSensoryMapParallel nns nna nns.Senses)
                                                (getthreads nns)
                                                currentfunction
                                                newsamplecount
                                                newsamples
                                                ispaused
                                                newpreviousduration
                                                currentdatetime
                                        | None ->
                                            soevnnreader.Close()
                                            basicsteps()
                                    | _ ->
                                        soevnnreader.Close()
                                        basicsteps()
                                | None ->
                                    soevnnreader.Close()
                                    basicsteps()
                            | None ->
                                basicsteps()
                        //with
                        //    | error -> 
                        //        basicsteps()

                    /////////////////////////////////////////////////////////////////////////////////////////////////////////
                    // Error: Does not load Soevnn correctly even though all save/load tests come back successful. 
                    //        This implies that it's the post processing that is causing the issue, except that 
                    //        the nervous system isn't modified, just the neuron array, address array, and sensory map.
                    /////////////////////////////////////////////////////////////////////////////////////////////////////////
                    | TestLoadSoevnn loadfrom ->
                        try
                            let maybealoadfile = 
                                match loadfrom with
                                | LoadFromFileName loadfilename ->
                                    LoadFile "soevnn" loadfilename
                                    |> Option.map (fun fs -> fs :> Stream)
                                | LoadFromStream stream ->
                                    Some stream

                            match maybealoadfile with
                            | Some soevnnreader ->
                                let sr = soevnnreader
                                let newns = Soevnn.Serialization.LoadNervousSystem sr
                                match newns with
                                | Some (nns,nn) ->
                                    let nna = nn
                                    let newnna = Array.map (fun (neuraladdress,neuron : Neuron) -> (neuraladdress,new Neuron(neuron.NeuralType,neuron.Dendrites,neuron.Axon,neuron.CurrentAccumRate, neuron.ExpectedAccumRate, neuron.CurrentAccum, neuron.ExpectedAccum, neuron.Variance))) nna
                                    processsteps 
                                        (iteration + 1)
                                        maxiteration
                                        currentdatetime
                                        (Array.mapi (fun i (a,_) -> (a,i)) nn |> Map.ofArray)
                                        nns
                                        nna
                                        newnna
                                        (CreateSensoryMapParallel nns nna nns.Senses)
                                        (getthreads nns)
                                        currentfunction
                                        samplecount
                                        samples
                                        ispaused
                                        previousduration
                                        (if ispaused then currentdatetime else sessionstarttime)
                                | None ->
                                    basicsteps()
                            | None ->
                                basicsteps()
                        with
                            | error ->
                                basicsteps()
                    | TestSetFunction(newfunctionname,newfunction) ->
                        processsteps
                            (if ispaused then iteration else iteration + 1)
                            maxiteration
                            currentdatetime
                            addresses
                            nervoussystem
                            newneurons
                            neurons
                            newsensorymap
                            threads
                            (newfunctionname,newfunction)
                            samplecount
                            samples
                            ispaused
                            previousduration
                            (if ispaused then currentdatetime else sessionstarttime)
                    | TestGetFunction(result) ->
                        result.Reply(currentfunction)
                        basicsteps()
                    | TestGetRemainingIterations(result) ->
                        result.Reply(match maxiteration with | None -> None | Some value -> value - iteration |> Some)
                        basicsteps()
                    | TestGetSampleCount(result) ->
                        result.Reply(samplecount)
                        basicsteps()

                else
                    basicsteps()
            else
                basicsteps()
        else
            processstep iteration addresses neurons newneurons smap threads (snd currentfunction)
            |> (fun (a,b,c) -> 
                samples.Enqueue(c)
                if samples.Count > samplecount then samples.Dequeue() |> ignore
                (a,b,(samples.ToArray())))
        
    let trainedneurons,trainedsmap, samplearrays =
        let newneurons = Array.map (fun (neuraladdress,neuron : Neuron) -> (neuraladdress,new Neuron(neuron.NeuralType,neuron.Dendrites,neuron.Axon,neuron.CurrentAccumRate, neuron.ExpectedAccumRate, neuron.CurrentAccum, neuron.ExpectedAccum, neuron.Variance))) initialneurons
        processsteps 1 trainingsteps initialdatetime initialaddresses ns initialneurons newneurons (CreateSensoryMapParallel ns initialneurons senses) (getthreads ns) (fname, inputfunction) testingsteps (new System.Collections.Generic.Queue<float[]>()) false TimeSpan.Zero initialdatetime


    let functionsamples = Array.map (fun (a:float[]) -> a.[0]) samplearrays
    let mimicsamples = Array.map (fun (a:float[]) -> a.[1]) samplearrays
    let errorsamples = Array.map (fun (a:float[]) -> a.[1] - a.[0]) samplearrays
    let absoluteerrorsamples = Array.map (fun (a:float[]) -> a.[1] - a.[0] |> abs) samplearrays
    
    let functionstats = Stats functionsamples
    let mimicstats = Stats mimicsamples
    let errorstats = Stats errorsamples
    let absoluteerrorstats = Stats absoluteerrorsamples

    if (not silent) then
        printbn "Output Data"
        printbn "%A" mimicsamples
        printbn "Minimum output: %f" mimicstats.Minimum
        printbn "Maximum output: %f" mimicstats.Maximum
        printbn "Range of output: %f" mimicstats.Range
        printbn "Median output: %f" mimicstats.Median
        printbn "Average output: %f" mimicstats.Average
        printbn "Variance of output: %f" mimicstats.Variance
    
        printbn "Error Data"
        printbn "%A" errorsamples
        printbn "Minimum error: %f" errorstats.Minimum
        printbn "Maximum error: %f" errorstats.Maximum
        printbn "Range of error: %f" errorstats.Range
        printbn "Median error: %f" errorstats.Median
        printbn "Average error: %f" errorstats.Average
        printbn "Variance of error: %f" errorstats.Variance

    match messagequeue with
    | Some mq -> 
        mq.Enqueue(sprintf "Output Data")
        mq.Enqueue(sprintf "%A" mimicsamples)
        mq.Enqueue(sprintf "Minimum output: %f" mimicstats.Minimum)
        mq.Enqueue(sprintf "Maximum output: %f" mimicstats.Maximum)
        mq.Enqueue(sprintf "Range of output: %f" mimicstats.Range)
        mq.Enqueue(sprintf "Median output: %f" mimicstats.Median)
        mq.Enqueue(sprintf "Average output: %f" mimicstats.Average)
        mq.Enqueue(sprintf "Variance of output: %f" mimicstats.Variance)
        
        mq.Enqueue(sprintf "Error Data")
        mq.Enqueue(sprintf "%A" errorsamples)
        mq.Enqueue(sprintf "Minimum error: %f" errorstats.Minimum)
        mq.Enqueue(sprintf "Maximum error: %f" errorstats.Maximum)
        mq.Enqueue(sprintf "Range of error: %f" errorstats.Range)
        mq.Enqueue(sprintf "Median error: %f" errorstats.Median)
        mq.Enqueue(sprintf "Average error: %f" errorstats.Average)
        mq.Enqueue(sprintf "Variance of error: %f" errorstats.Variance)

    | None -> ()

    match logfile with
    | Some writer -> 
        fprintfn writer "Input Function: %s" fname

        fprintfn writer "Output Data"
        fprintfn writer "%A" mimicsamples
        fprintfn writer "Minimum output: %f" mimicstats.Minimum
        fprintfn writer "Maximum output: %f" mimicstats.Maximum
        fprintfn writer "Range of output: %f" mimicstats.Range
        fprintfn writer "Median output: %f" mimicstats.Median
        fprintfn writer "Average output: %f" mimicstats.Average
        fprintfn writer "Variance of output: %f" mimicstats.Variance
        
        fprintfn writer "Error Data"
        fprintfn writer "%A" errorsamples
        fprintfn writer "Minimum error: %f" errorstats.Minimum
        fprintfn writer "Maximum error: %f" errorstats.Maximum
        fprintfn writer "Range of error: %f" errorstats.Range
        fprintfn writer "Median error: %f" errorstats.Median
        fprintfn writer "Average error: %f" errorstats.Average
        fprintfn writer "Variance of error: %f" errorstats.Variance

    | None -> ()
    let timeelapsed = initialdatetime.Subtract(DateTime.UtcNow)
    if not silent then
        printbn "Duration: %s" <| timeelapsed.ToString()

    match logfile with
    | Some value -> value.Flush(); value.Dispose()
    | None -> ()