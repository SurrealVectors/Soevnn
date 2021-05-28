module Soevnn.Parallel.Core
open Soevnn.Core
open Soevnn.Utilities.PathwayMap
open Soevnn.Parallel.PathwayMap
open Soevnn.Utilities.Partitions
//let Parallel = System.Threading.Tasks.Parallel
            

type SensoryMapParallel = Map<NeuralAddress,(Map<int,NeuralAddress array> * Map<NeuralAddress,int>)>


let CreateSensoryMapParallel  (nervoussytem : NervousSystem) (neuralmap : (NeuralAddress * Neuron) array) (senses : NeuralAddress list) : SensoryMapParallel =
    let nodes =
        neuralmap
        |> Array.Parallel.map (fun (na : NeuralAddress, n : Neuron) -> (na, {location = na; data = n; adjecent = n.Dendrites |> List.unzip3 |> (fun (a,_,_) -> a)}))
        |> InvertDirectionParallel
        |> Map.ofArray
    let pathmaps =
        senses
        |> List.toArray
        |> Array.Parallel.map (PathMapParallel nodes 5 3)
    let depthsmaps =
        pathmaps
        |> Array.Parallel.map 
            (fun (_,nmbd) -> 
                nmbd |> Map.toArray |> Array.Parallel.collect 
                    (fun (depth : int, nal : NeuralAddress array) -> nal |> Array.Parallel.map (fun na -> (na,depth)))
                |> Map.ofArray
            )
    Array.Parallel.init senses.Length (fun i -> (senses.[i],(pathmaps.[i] |> snd, depthsmaps.[i])))
    |> Map.ofArray

let CreateSensoryConnectionMapParallel (cluster : NeuralAddress list) (sensoryadaptions : SensoryAdaption list) (sensorymaps : SensoryMapParallel) = 
    
    let listlast (l : _ list) = l.[l.Length-1]
    let duplicatelastitem (f : _ -> _) (l : (_ * _) list) = (l |> listlast |> fst |> f,l |> listlast |> snd) :: l
    
    let getratio (smbd : Map<int,NeuralAddress array>, smbn : Map<NeuralAddress,int>) =
        cluster
        |> List.fold
            (fun (count,state) (n:NeuralAddress) ->
                if smbn.ContainsKey n then (count+1, state + smbn.[n]) else (count,state))
            (0,0)
        |> (fun (count,a) -> if count > 0 then (float a * float a / float count) else 1.0)
    let ratios =
        sensorymaps
        |> Map.toList
        |> List.map (snd >> getratio)
    let probs =
        cluster
        |> List.map
            (fun n ->
                (sensoryadaptions
                |> List.fold
                    (fun (index,accum) (sa) -> 
                        let smbd, smbn = sensorymaps.[sa.Sense]
                        (
                        index+1,
                        if smbn.ContainsKey n then
                            let depth = smbn.[n]
                            if depth >= smbd.Count - 1 then
                                sa.Weight * sa.WidthHeightRatio / ratios.[index]
                            else
                                sa.Weight * sa.WidthHeightRatio / (float (smbd.[depth+1].Length+1) / float (depth+1))
                        else
                            0.0)
                        )
                    (0,0.0)
                )|>snd,
                n)
        |> List.filter (fun (w,_) -> w > 0.0)
    let indices =
        if probs.IsEmpty then
            []
        else if probs.Length = 1 then
            [(0.0, snd probs.Head); probs.Head]
        else
            (0.0, snd probs.Head) ::
            [for i in [1 .. probs.Length - 1] do 
                yield (
                    (fst probs.[i - 1], snd probs.[i])
                )
            ]
            |> List.scan (fun (accum, _) (weight, i) -> (weight+accum,i)) (0.0, snd probs.Head)
            |> duplicatelastitem (fun i -> i + (probs |> listlast |> fst))
    if indices.IsEmpty then
        None
    else
        AddItemsHigh
            indices
            (Item(snd probs.Head))
        |> Balance |> PartitionTree |> Some




let SelectNeuronParallel (rand : System.Random) (neuron : NeuralAddress) (ns : NervousSystem) (map : ConnectivityMap) (sensorymap : SensoryMapParallel) : NeuralAddress =
    let defaultto0 option = match option with | Some value -> value | None -> 0
    let defaultto0d option = match option with | Some value -> value | None -> decimal 0.0
    
    let relation, structuresmap = map.[defaultto0d map.HighestKey * decimal(rand.NextDouble())];
    let groupsmap = structuresmap.[rand.Next(structuresmap.HighestKey|>defaultto0)]
    let clustersmap = groupsmap.[rand.Next(groupsmap.HighestKey|>defaultto0)]
    let nr, s, g, c = clustersmap.[defaultto0d clustersmap.HighestKey * decimal(rand.NextDouble())]
    let nt = ns.NeuralTypeOf neuron
    let rn () = NeuralAddress(s,g,c,rand.Next(ns.Structures.[s].Groups.[g].NeuralType.ClusterSize))
    if rand.NextDouble() < nt.SensoryAdaptionRate then
        let scm = CreateSensoryConnectionMapParallel (ns.Structures.[s].Groups.[g].Clusters.[c].Neurons) nt.SensoryAdaptions sensorymap
        if scm.IsSome then
            scm.Value.[rand.NextDouble() * scm.Value.HighestKey.Value]
        else
            rn()
    else
        rn()


let CreateSensoryMusclePathConnectionParallel (rand : System.Random) (ns : NervousSystem) (cmap : ConnectivityMap) (sensorymap : SensoryMapParallel) (neuraladdress : NeuralAddress) (neuron : Neuron) =
    let randvalue = rand.NextDouble()
    let rec connection index results =
        if index < neuron.NeuralType.AxonalSynapses - neuron.Dendrites.Length && (index < neuron.NeuralType.DendritricSynapsesMin - neuron.Dendrites.Length || randvalue < neuron.NeuralType.AdaptionRate * System.Math.Pow(0.5, float index)) then
            let selectedneuron =
                SelectNeuronParallel
                    rand
                    neuraladdress
                    ns
                    cmap
                    sensorymap
            (
                selectedneuron,
                (rand.NextDouble() < neuron.NeuralType.DirectInverseBalance),
                ref 1.0
            ) 
            :: results
            |> connection (index + 1)
        else
            results
    connection 1 neuron.Dendrites
    
let CreateConnectionParallel (rand : System.Random) (ns : NervousSystem) (cmap : ConnectivityMap) (sensorymap : SensoryMapParallel) (neuraladdress : NeuralAddress) (neuron : Neuron) =
    let randvalue = rand.NextDouble()
    let rec connection index results =
        if index < neuron.NeuralType.AxonalSynapses - neuron.Dendrites.Length && (index < neuron.NeuralType.DendritricSynapsesMin - neuron.Dendrites.Length || randvalue < neuron.NeuralType.AdaptionRate * System.Math.Pow(0.5, float index)) then
            let selectedneuron =
                SelectNeuronParallel
                    rand
                    neuraladdress
                    ns
                    cmap
                    sensorymap
            (
                selectedneuron,
                (rand.NextDouble() < neuron.NeuralType.DirectInverseBalance),
                ref 1.0
            ) 
            :: results
            |> connection (index + 1)
        else
            results
    connection 1 neuron.Dendrites


/// <summary> Processes a single neuron as a part of a parallelly processed nervous system. </summary>
/// <see cref="ProcessThread"/>
let ProcessNeuronParallel (neuraladdress : NeuralAddress) (neuron : Neuron) (newneuron : Neuron) (nervoussytem : NervousSystem) (connectivitymap : ConnectivityMap) (sensorymap : SensoryMapParallel) (map : NeuralAddress -> Neuron) (rand : System.Random) (sense : float option) =
    let nt = neuron.NeuralType
    let input = neuron.Dendrites
    let count = max 1 <| input.Length + if sense.IsSome then 1 else 0
    let sum = 
        List.sumBy 
            (fun (na,isinverse,mult) -> 
                if isinverse then
                    match map(na).NeuralType.NeuralType with
                    | NtOffset -> -map(na).Axon * !mult
                    | NtScaling -> (if map(na).Axon = 0.0 then 1.0 else 1.0 / map(na).Axon) * !mult
                    | NtSpiking -> (1.0 - map(na).Axon) * !mult
                else
                    map(na).Axon * !mult) 
            input
        + if sense.IsSome then sense.Value else 0.0
    
    let newcurrentaccum,newexpectedaccum, newvariance, newaxon =
        SubprocessLearn neuron sum count
    newneuron.CurrentAccum <- newcurrentaccum
    newneuron.ExpectedAccum <- newexpectedaccum
    newneuron.Variance <- newvariance
    newneuron.Axon <- newaxon
    newneuron.Dendrites <-
        SubprocessAdapt map rand neuron sum count
    newneuron.Dendrites <-
        CreateConnectionParallel 
            rand 
            nervoussytem 
            connectivitymap 
            sensorymap
            neuraladdress 
            newneuron
    SubProcessBalanceSynapses newneuron input map sum count
    ()


/// <summary> Creates the initial synaptic connections for all the neurons in the NervousSystem. </summary>
let CreateConnectionsParallel (rand : System.Random) (ns : NervousSystem) (neurons : (NeuralAddress * Neuron) array) (cmap : ConnectivityMap) (sensorymap : SensoryMapParallel) =
    Array.Parallel.map 
        (fun (na: NeuralAddress, n: Neuron) ->
            (
                na,
                Neuron(
                    n.NeuralType,
                    (CreateConnectionParallel rand ns cmap sensorymap na n),
                    n.Axon,
                    n.CurrentAccumRate,
                    n.ExpectedAccumRate,
                    n.CurrentAccum,
                    n.ExpectedAccum,
                    n.Variance
                )
            )
        )
        neurons

/// <summary> Creates an array of neural addresses separated into lists depending on the kind of neural pathway of each Neuron within a NervousSystem. </summary>
let getthreads (ns : NervousSystem)  =
    let rec getneuraltypes (sl : NeuralStructure list) (results : NeuralType list) = 
        match sl with
        | [] -> results
        | head :: tail -> 
            [for g in head.Groups do if List.contains g.NeuralType results |> not then yield g.NeuralType] @ results
            |> getneuraltypes sl.Tail
    Array.concat
        [
            [|for s in ns.Structures do 
                for g in s.Groups do 
                    match g.NeuralType.Pathway with 
                    | NpCluster -> 
                        for c in g.Clusters do yield (c.Neurons)
                    | NpGroup -> yield [for c in g.Clusters do yield! (c.Neurons)]
                    | _ -> ()
            |];
        
            [|for s in ns.Structures do 
                yield [for g in s.Groups do 
                        match g.NeuralType.Pathway with 
                        | NpStructure ->
                            for c in g.Clusters do yield! (c.Neurons)
                        | _ -> ()
                ]
            |];

            [|for nt in getneuraltypes ns.Structures [] do 
                match nt.Pathway with
                | NpType ->
                    yield [
                        for s in ns.Structures do
                            for g in s.Groups do
                                if g.NeuralType = nt then
                                    for c in g.Clusters do
                                        yield! c.Neurons
                    ]
                | _ ->  ()

            |]
        ]
        
/// <summary>  Processes a single thread of neurons sequentially. Neural outputs are read via the write array buffer if already updated within this thread, otherwise it uses the read array buffer. 
/// This allows information to travel quickly along threads in a safe manner.  </summary>
let rec ProcessThread (nervoussytem : NervousSystem) (connectivitymap : ConnectivityMap) (sensorymap : SensoryMapParallel) (rand : System.Random) (addresses : Map<NeuralAddress,int>) (neurons : (NeuralAddress * Neuron) array) (newneurons : (NeuralAddress * Neuron) array) (senses : Map<NeuralAddress,float>) (muscles : Map<NeuralAddress,int>) (toprocess : NeuralAddress list) (processedneurons : Map<NeuralAddress,Neuron>) =
    match toprocess with
    | (na)::tail ->
        let n = snd neurons.[addresses.[na]]
        let newn = snd newneurons.[addresses.[na]]
        let indexer a =
            if processedneurons.ContainsKey(a) then
                processedneurons.[a]
            else
                snd neurons.[addresses.[a]]
        let newna, _ = (na, ProcessNeuronParallel na n newn nervoussytem connectivitymap sensorymap indexer rand (if senses.ContainsKey(na) then Some(senses.[na]) else None))
        ProcessThread nervoussytem connectivitymap sensorymap rand addresses neurons newneurons senses muscles tail (processedneurons.Add (newna,newn)) 
    | [] -> ()

/// <summary>Processes one step of all neural activity in parallel.</summary>
/// <param name="nervoussytem">The Nervous System. </param>
/// <param name="connectivitymap">The probability map of particular synaptic connections forming.</param>
/// <param name="addresses">The map from neural addresses to the neuron array indices.</param>
/// <param name="neurons">The neurons to be processed.</param>
/// <param name="rand">The random number generator to be used.</param>
/// <param name="senses">The neural addresses of senses and their inputs.</param>
/// <param name="muscles">The map from neural addresses the muscle array indices.</param>
/// <returns>The new neurons and the outputs of muscles.</returns>
let ProcessAllParallel (nervoussytem : NervousSystem) (connectivitymap : ConnectivityMap) (sensorymap : SensoryMapParallel) (rand : System.Random) (addresses : Map<NeuralAddress,int>) (neurons : (NeuralAddress * Neuron) array) (newneurons : (NeuralAddress * Neuron) array) (threads : NeuralAddress list array) (senses : Map<NeuralAddress,float>) (muscles : Map<NeuralAddress,int>) = 
    
    threads
    |> Array.Parallel.iter (fun toprocess ->
        ProcessThread nervoussytem connectivitymap sensorymap rand addresses neurons newneurons senses muscles toprocess Map.empty
        )

        

    let muscleresults =
        Array.Parallel.choose
            (fun (na : NeuralAddress, n : Neuron) -> if Map.containsKey na muscles then Some(n.Axon) else None)
            newneurons
    (newneurons,muscleresults)


let CreateIndexedNeuronsParallel (ns : NervousSystem) =
    let m =
        [for s in ns.Structures do 
            yield!
                [for g in s.Groups do
                    yield!
                        [for c in g.Clusters do 
                            yield!
                                [for n in c.Neurons do 
                                    yield
                                        (
                                            n,
                                            Neuron(
                                                g.NeuralType,
                                                [],
                                                1.0,
                                                g.NeuralType.CurrentAccumRate, 
                                                g.NeuralType.ExpectedAccumRate,
                                                1.0,
                                                1.0,
                                                1.0
                                            )
                                        )
                                ]
                        ]
                ]
        ]
        |> Map.ofList
    let r =
        getthreads ns
        |> List.concat
    Array.Parallel.init r.Length (fun i -> (r.[i],m.[r.[i]]))