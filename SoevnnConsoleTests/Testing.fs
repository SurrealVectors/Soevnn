﻿module Soevnn.Console.Testing


open System
open Soevnn.Core
open Soevnn.Parallel.Core
open System.IO
open Soevnn.Serialization
open Microsoft.FSharp.Reflection
open Soevnn.Console.Utilities

/// <summary> Gets the kind of neural pathway by index. </summary>
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

/// <summary> A list of input functions to text the neural networks against. </summary>
let inputfunctions =
    Map.ofList
        [
            ("constant", (fun _ -> 100.0));
            ("sin", (fun i -> 100.0 * sin (0.1 * float i)));
            ("mod", (fun i -> 100.0 * Math.IEEERemainder(0.1 * float i, 1.0)))
        ]

/// <summary> A set of statistics for a given sequence of data. </summary>
type Stats(data : float seq) =
    member this.Minimum = Seq.min data
    member this.Maximum = Seq.max data
    member this.Average = Seq.average data
    member this.Median = 
        let ordereddata = Seq.sort data
        Seq.item (Seq.length ordereddata / 2) ordereddata
    member this.Variance = Seq.averageBy (fun d -> pown (d - this.Average) 2) data |> Math.Sqrt
    member this.Range = this.Maximum - this.Minimum

/// <summary> The kind of place to load from. </summary>
type LoadFrom =
| LoadFromFileName of string
| LoadFromStream of Stream

/// <summary> The kind of place to save to. </summary>
type SaveTo =
| SaveToFileName of string
| SaveToBinaryWriter of BinaryWriter

/// <summary> A command to give to a particular test. </summary>
type TestCommand =
| TestPause // Pauses a running test.
| TestContinue // Continues a paused test.
| TestStep // Runs a single step of a paused test.
| TestSaveTest of string // Saves a test.
| TestSaveSoevnn of string // Saves a test's Soevnn.
| TestSetRemainingIterations of int option // Set how many more steps to process before pausing. If no number is given, instead continue running indefinitely.
| TestGetRemainingIterations of int option AsyncReplyChannel // Gets how many more steps to process before pausing. If no number is given, this indicate the test will continue to run indefinitely.
| TestSetSampleCount of int // Sets how many of the most recent samples should be gathered. 
| TestGetSampleCount of int AsyncReplyChannel // Returns how many of the most recent samples are to be gathered.
| TestGetSamples of (Stats * Stats * Stats * Stats) AsyncReplyChannel // Gets the statistics of the most recent samples. The stats are of the input function, the output of the Soevnn, the signed errors, and the unsigned errors.
| TestGetProgress of ((int * TimeSpan) * (int * TimeSpan) option) AsyncReplyChannel // Gets the progess of the test in the form of: (CurrentIterations, TimeLapsed), (TotalIterations, EstimatedTimeRemaining)
| TestLoadTest of LoadFrom // Loads a test.
| TestLoadSoevnn of LoadFrom // Loads a Soevnn.
| TestSetFunction of string * (int -> float) // Sets the input function.
| TestGetFunction of (string * (int -> float)) AsyncReplyChannel // Gets the input function.

/// <summary> Prints the statistics of a particular sample. </summary>
let printsamples (samples : float [] option) (samplestats : Stats) (label : string) =
    printbn "%s" label
    if samples.IsSome then printbn "%A" samples.Value
    printbn "Minimum output: %f" samplestats.Minimum
    printbn "Maximum output: %f" samplestats.Maximum
    printbn "Range of output: %f" samplestats.Range
    printbn "Median output: %f" samplestats.Median
    printbn "Average output: %f" samplestats.Average
    printbn "Variance of output: %f" samplestats.Variance
    
/// <summary> Logs the samples and the their statistics. </summary>
let logsamples (writer) (samples : float [] option) (samplesstats : Stats) (label : string) =
    fprintfn writer "%s" label
    if samples.IsSome then fprintfn writer "%A" samples.Value
    fprintfn writer "Minimum output: %f" samplesstats.Minimum
    fprintfn writer "Maximum output: %f" samplesstats.Maximum
    fprintfn writer "Range of output: %f" samplesstats.Range
    fprintfn writer "Median output: %f" samplesstats.Median
    fprintfn writer "Average output: %f" samplesstats.Average
    fprintfn writer "Variance of output: %f" samplesstats.Variance

/// <summary> Initiates the a test of a Soevnn. Contains concurrent queues for passing messages to and from the ongoing test. </summary>
let TestNervousSystem neuraltypes neuralstructures seed (trainingsteps : int option) (testingsteps : int) (fname, inputfunction) (logfile: StreamWriter option) silent (messagequeue : System.Collections.Concurrent.ConcurrentQueue<string> option) (commandqueue : MailboxProcessor<TestCommand> option) = 
    let initialdatetime = DateTime.UtcNow
    if not silent then
        printbn "Start Time: %s" <| DateTime.Now.ToShortTimeString()
    
    // The neurons handling inputs and outputs, referenced via neural addresses. 
    let input = NeuralAddress(0,0,0,0) // input
    let error = NeuralAddress(0,0,0,2) // input
    let mimic = NeuralAddress(0,0,0,1) // output
    let senses = [input;error] // list of inputs
    let rand = match seed with | Some(seedvalue) -> System.Random(seedvalue) | None -> System.Random() // Random number generator used for forming and breaking synaptic connections.
    
    


    let nt = // List of neural types.
        List.map 
            (fun (intrainterbalancegroup, intrainterbalancestructure, intrainterbalancecluster, directinversebalance, clustersize, currentaccumrate, expectedaccumrate, adaptionrate, synapsecountmin, synapsecountmax, pathwaytype) -> 
                (NeuralType(intrainterbalancegroup,intrainterbalancestructure,intrainterbalancecluster, directinversebalance,synapsecountmin, synapsecountmax,synapsecountmax,adaptionrate,currentaccumrate,expectedaccumrate,clustersize,NtOffset,getpathway pathwaytype,0.25,[SensoryAdaption(input, 2.0, 1.0); SensoryAdaption(error, 0.5, 0.25)])))
            neuraltypes
    let nsl = // List of neural structures.
        neuralstructures
        |> List.choose
            (fun ns ->
                ns
                |> SomeIfForAll (snd >> IndexInRange nt)
                |> Option.map (List.map (fun (cnt,index) -> (cnt,nt.[index])))
            )

    let ns = CreateNervousSystemFromStructures nsl senses [mimic] // The nervous system.
    let cmap = CreateConnectivityMap ns // The connectivity map.
    let indexedneurons = (CreateIndexedNeuronsParallel ns)
    let smap = CreateSensoryMapParallel ns (indexedneurons) senses 
    let initialneurons= 
        CreateConnectionsParallel rand ns indexedneurons (cmap) (smap) 
    let initialaddresses = // A map from neural addresses to array indices of the neurons.
        initialneurons |> Array.mapi (fun i (k,v) -> (k,i)) |> Map.ofArray
    let processstep iteration addresses neurons newneurons smap threads currentfunction  : ((NeuralAddress * Neuron) []) * SensoryMapParallel * float [] = // Processes a single step.
        (
            async { return (ProcessAllParallel ns cmap smap rand addresses neurons newneurons threads (Map.ofList [(input, inputfunction iteration);(error,(snd neurons.[addresses.[mimic]]).Axon - currentfunction (iteration - 1))]) (Map.ofList [(mimic,0)]))} // Processes the neurons. 
            ,
            async { return (CreateSensoryMapParallel ns neurons senses)} // Updates the sensory map.
        ) 
        |> (fun (procall,createsmap) -> 
            let taskprocall = Async.StartAsTask procall
            let taskcreatesmap = Async.StartAsTask createsmap
            (
                taskprocall.GetAwaiter().GetResult(),   // Wait for...
                taskcreatesmap.GetAwaiter().GetResult() // ... these results.
            ) 
        )
        |> (fun ((a,b),c) -> 
            (a,c,Array.concat [[|(inputfunction iteration)|];b])) 

    let rec processsteps (iteration : int) (maxiteration : int Option) previousdatetime addresses (nervoussystem : NervousSystem) (neurons : (NeuralAddress*Neuron)[]) (newneurons : (NeuralAddress*Neuron)[]) smap threads currentfunction samplecount (samples : System.Collections.Generic.Queue<float []>) ispaused (previousduration : TimeSpan) (sessionstarttime : DateTime) : ((NeuralAddress * Neuron) []) * SensoryMapParallel * float [] [] = // The ongoing process that manages the test.
        let currentdatetime = if ispaused then sessionstarttime else DateTime.UtcNow
        let currenttimelapsed = TimeSpan.FromTicks(currentdatetime.Subtract(sessionstarttime).Ticks + previousduration.Ticks)
        let setl = 
            match maxiteration with
            | Some maxiter ->
                TimeSpan.FromTicks(currenttimelapsed.Ticks * int64 (maxiter - iteration) / int64 iteration) |> Some // Estimates the remaining time for a test with a set number of steps.
            | None -> None
        
        let etls = // Converts the remaining time to a user-readable format.
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
        
        if maxiteration.IsSome && (100000 * iteration / maxiteration.Value) % 100  = 0 then // Checks if the test has concluded with the requested number of steps.
            if (not silent) && messagequeue.IsNone then
                printbn "%f%% Training Complete / %s Estimated Time Remaining" (float(1000 * iteration / maxiteration.Value) * 0.1) etls
            if messagequeue.IsSome then
                messagequeue.Value.Enqueue(sprintf "%f%% Training Complete / %s Estimated Time Remaining" (float(1000 * iteration / maxiteration.Value) * 0.1) etls)
        if (match maxiteration with | None -> true | Some maxiter when (iteration < maxiter) -> true | _ -> false) then // Checks if the test is to continue.
            let newneurons,newsensorymap,sample = 
                    if ispaused then
                        neurons,smap,[||]
                    else
                        processstep iteration addresses neurons newneurons smap threads (snd currentfunction)
            if not ispaused then // If there is another sample, collect it.
                samples.Enqueue(sample)
                if samples.Count > samplecount then samples.Dequeue() |> ignore
            let inline basicsteps () = // The default parameters to go to the next step.
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
            if commandqueue.IsSome then // Processes the next command, if there is one.
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
                    | TestContinue -> // Continues the test from a paused state.
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
                    | TestSetSampleCount newsamplecount -> // Sets how many more steps to process before pausing. If no number is given, instead continue running indefinitely.
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
                    | TestPause -> // Pauses the test if it's running.
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
                    | TestStep -> // Process a single step while in the paused state.
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
                    | TestGetSamples results -> // Gets the statistics of the most recent samples.
                        let samplearrays = samples.ToArray()
                        let functionsamples = Array.map (fun (a:float[]) -> a.[0]) samplearrays
                        let mimicsamples = Array.map (fun (a:float[]) -> a.[1]) samplearrays
                        let errorsamples = Array.map (fun (a:float[]) -> a.[1] - a.[0]) samplearrays
                        let absoluteerrorsamples = Array.map (fun (a:float[]) -> (a.[1] - a.[0]) |> abs) samplearrays
                        results.Reply(Stats functionsamples,Stats mimicsamples, Stats errorsamples, Stats absoluteerrorsamples)
                        basicsteps()
                    | TestGetProgress results -> // Gets the progress of the test if it has a set number of steps to process.
                        match (maxiteration,setl) with
                        | (Some miter,Some etl) ->
                            results.Reply((iteration,currenttimelapsed+previousduration),Some(miter,etl))
                        | _ ->
                            results.Reply((iteration,currenttimelapsed+previousduration),None)
                        basicsteps()
                    | TestSetRemainingIterations newremainingiterations -> // Sets how many more steps to process before pausing.
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
                    | TestSetFunction(newfunctionname,newfunction) -> // Sets the input function of the test.
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
                    | TestGetFunction(result) -> // Gets the input function of the test.
                        result.Reply(currentfunction)
                        basicsteps()
                    | TestGetRemainingIterations(result) -> // Gets the remaining number of steps if there is a set number of steps to process.
                        result.Reply(match maxiteration with | None -> None | Some value -> value - iteration |> Some)
                        basicsteps()
                    | TestGetSampleCount(result) -> // Gets the number of samples to gather.
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