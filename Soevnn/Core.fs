// Self Organizing Estimated Variance Neural Network
module Soevnn.Core
open System.Collections.Concurrent
open Soevnn.Utilities.Partitions
open Soevnn.Utilities.PathwayMap
open MessagePack

/// <summary> Provides a system and session agnostic way of referencing a neuron. </summary>
[<MessagePackObject>]
type NeuralAddress(structureindex : int, groupindex : int, clusterindex : int, neuronindex : int) =
    struct
        [<Key(0)>]
        member _.StructureIndex : int = structureindex
        [<Key(1)>]
        member _.GroupIndex : int = groupindex
        [<Key(2)>]
        member _.ClusterIndex : int = clusterindex
        [<Key(3)>]
        member _.NeuronIndex : int = neuronindex
        override this.ToString() = Printf.sprintf "<%i,%i,%i,%i>" this.StructureIndex this.GroupIndex this.ClusterIndex this.NeuronIndex
    end

/// <summary> Marks which activation function should be used. </summary>
[<MessagePackObject>]
type NeuralTypeMethod =
    /// <summary> Unsupported. Provides a binary output. </summary>
    | NtSpiking
    /// <summary> Sums inputs and compares averages via subtraction. </summary>
    | NtOffset
    /// <summary> Multiplies inputs compares averages via division. </summary>
    | NtScaling

// NeuralState is unused and should be removed.
/// <summary> Deprecated. </summary>
[<MessagePackObject>]
type NeuralState =
    | NsSpiking
    | NsOffset
    | NsScaling

/// <summary> Marks which kind of neural pathway the neuron is a part of. Neural pathways are a means of partitioning the neurons for parallelism. </summary>
[<MessagePackObject>]
type NeuralPathway =
    /// <summary> The neuron is a part of a thread consisting of it's own cluster. </summary>
    | NpCluster
    /// <summary> The neuron is a part of a thread consisting of it's own group. </summary>
    | NpGroup
    /// <summary> The neuron is a part of a thread consisting of neurons from it's own structure that aren't already a part of any other neural pathways. </summary>
    | NpStructure
    /// <summary> The neuron is a part of a thread consisting of it's own type across structures. </summary>
    | NpType

// This doesn't actually need to be for a sense, and can be used for any neuron. Could be useful to ensure connected with important parts of the nervous system. TO CONSIDER: Revise naming to reflect that?
/// <summary> Promotes path-connectedness between the neuron and a sense. </summary> 
[<MessagePackObject>]
type SensoryAdaption( sense : NeuralAddress, weight : float, widthheightratio : float) = 
    [<Key(0)>]
    member _.Sense = sense // Which sense to connect to.
    [<Key(1)>]
    member _.Weight = weight // The weight of this potential selection when the neuron forms a new connection.
    [<Key(2)>]
    member _.WidthHeightRatio = widthheightratio // The breadth vs depth of the connection tree with the sense as the root. 
    override this.GetHashCode() =
      hash (this.Sense, this.Weight, this.WidthHeightRatio)
    override this.Equals(other) =
      match other with
      | :? SensoryAdaption as other -> (this.Sense, this.Weight, this.WidthHeightRatio) = (other.Sense, other.Weight, other.WidthHeightRatio)
      | _ -> false
    interface System.IComparable with
        member this.CompareTo(other : obj) =
            match other with
            | :? SensoryAdaption as other -> 
                let this = (this.Sense, this.Weight, this.WidthHeightRatio)
                let other = (other.Sense, other.Weight, other.WidthHeightRatio)
                if this > other then
                    1
                else if this = other then
                    0
                else
                    -1
            | _ -> -1

/// <summary> A particular parameterization of the neurons. </summary>
[<MessagePackObject>]
type NeuralType(intrainterbalancegroup : float, intrainterbalancestructure : float, intrainterbalancecluster : float, directinversebalance : float, dendritricsynapsesmin : int, dendritricsynapsesmax : int, axonalsynapses : int, adaptionrate : float, currentaccumrate : float, expectedaccumrate : float, clustersize : int, neuraltype : NeuralTypeMethod, pathway : NeuralPathway, sensoryadaptionrate : float, sensoryadaptions : SensoryAdaption list) =
    [<Key(0)>]
    member _.IntraInterBalanceGroup : float = intrainterbalancegroup // When forming a connection, the preference of a connection within the neuron's group versus outside it.
    [<Key(1)>]
    member _.IntraInterBalanceStructure : float = intrainterbalancestructure // When forming a connection, the preference of a connection within the neuron's structure versus outside it.
    [<Key(2)>]
    member _.IntraInterBalanceCluster : float = intrainterbalancecluster // When forming a connection, the preference of a connection within the neuron's cluster versus outside it.
    [<Key(3)>]
    member _.DirectInverseBalance : float = directinversebalance // When forming a connection, the preference for whether the connection will be a direct connection or if the input should be inverted relative to the activation function.
    [<Key(4)>]
    member _.DendritricSynapsesMin : int = dendritricsynapsesmin // The minimum number of inputs.
    [<Key(5)>]
    member _.DendritricSynapsesMax : int = dendritricsynapsesmax // The maximum number of inputs.
    [<Key(6)>]
    member _.AxonalSynapses : int = axonalsynapses // The targeted number of outputs.
    [<Key(7)>]
    member _.AdaptionRate : float = adaptionrate // The rate at which neural connections are broken and formed.
    [<Key(8)>]
    member _.CurrentAccumRate : float = currentaccumrate // The short-term learn rate of the activation function.
    [<Key(9)>]
    member _.ExpectedAccumRate : float = expectedaccumrate // The long-term learn rate of the activation function.
    [<Key(10)>]
    member _.ClusterSize : int = clustersize // The size of the cluster to which the neuron belongs. 
    [<Key(11)>]
    member _.NeuralType : NeuralTypeMethod = neuraltype // Which activation function to use.
    [<Key(12)>]
    member _.Pathway : NeuralPathway = pathway // Which neural pathway the neuron is a part of. 
    [<Key(13)>]
    member _.SensoryAdaptionRate : float = sensoryadaptionrate // The rate at which path connections to the senses are formed.
    [<Key(14)>]
    member _.SensoryAdaptions : SensoryAdaption list = sensoryadaptions // The senses which the neuron should be path-connected to.
    override this.GetHashCode() =
      hash (this.IntraInterBalanceGroup, this.IntraInterBalanceStructure, this.IntraInterBalanceCluster, this.DirectInverseBalance, this.DendritricSynapsesMin, this.DendritricSynapsesMax, this.AxonalSynapses, this.AdaptionRate, this.CurrentAccumRate, this.ExpectedAccumRate, this.ClusterSize, this.NeuralType, this.Pathway, this.SensoryAdaptionRate, this.SensoryAdaptions)
    override this.Equals(other) =
      match other with
      | :? NeuralType as other -> 
        (this.IntraInterBalanceGroup, this.IntraInterBalanceStructure, this.IntraInterBalanceCluster, this.DirectInverseBalance, this.DendritricSynapsesMin, this.DendritricSynapsesMax, this.AxonalSynapses, this.AdaptionRate, this.CurrentAccumRate, this.ExpectedAccumRate, this.ClusterSize, this.NeuralType, this.Pathway, this.SensoryAdaptionRate, this.SensoryAdaptions) = 
            (other.IntraInterBalanceGroup, other.IntraInterBalanceStructure, other.IntraInterBalanceCluster, other.DirectInverseBalance, other.DendritricSynapsesMin, other.DendritricSynapsesMax, other.AxonalSynapses, other.AdaptionRate, other.CurrentAccumRate, other.ExpectedAccumRate, other.ClusterSize, other.NeuralType, other.Pathway, other.SensoryAdaptionRate, other.SensoryAdaptions)
      | _ -> false
    interface System.IComparable with
        member this.CompareTo(other : obj) =
            match other with
            | :? NeuralType as other -> 
                    let this = (this.IntraInterBalanceGroup, this.IntraInterBalanceStructure, this.IntraInterBalanceCluster, this.DirectInverseBalance, this.DendritricSynapsesMin, this.DendritricSynapsesMax, this.AxonalSynapses, this.AdaptionRate, this.CurrentAccumRate, this.ExpectedAccumRate, this.ClusterSize, this.NeuralType, this.Pathway, this.SensoryAdaptionRate, this.SensoryAdaptions)
                    let other = (other.IntraInterBalanceGroup, other.IntraInterBalanceStructure, other.IntraInterBalanceCluster, other.DirectInverseBalance, other.DendritricSynapsesMin, other.DendritricSynapsesMax, other.AxonalSynapses, other.AdaptionRate, other.CurrentAccumRate, other.ExpectedAccumRate, other.ClusterSize, other.NeuralType, other.Pathway, other.SensoryAdaptionRate, other.SensoryAdaptions)
                    if this > other then
                        1
                    else if this = other then
                        0
                    else
                        -1
            | _ -> -1
            
            
    
/// <summary> A cluster of neurons of the same neural type, or in other words the same parameterizations. </summary>
[<MessagePackObject; StructuralEquality; StructuralComparison>]
type NeuralCluster =
    struct
        [<Key(0)>]
        val Neurons : NeuralAddress list
        [<Key(1)>]
        val NeuronCount : int
        new(neurons : NeuralAddress list) = {Neurons=neurons; NeuronCount = neurons.Length}
        new(neurons : NeuralAddress list, neuroncount : int) = {Neurons=neurons; NeuronCount = neuroncount}
    end

/// <summary> A group of clusters, all of the same neural type. </summary>
[<MessagePackObject; StructuralEquality; StructuralComparison>]
type NeuralGroup =
    struct
        [<Key(0)>]
        val Clusters : NeuralCluster list
        [<Key(1)>]
        val NeuronCount : int
        [<Key(2)>]
        val NeuralType : NeuralType
        new(clusters:NeuralCluster list, neuraltype : NeuralType) = {Clusters=clusters;NeuralType=neuraltype; NeuronCount = List.sumBy (fun (c:NeuralCluster) -> c.NeuronCount) clusters}
        new(clusters:NeuralCluster list, neuroncount : int, neuraltype : NeuralType) = {Clusters=clusters;NeuralType=neuraltype; NeuronCount = neuroncount}
    end

/// <summary> A set of neural groups of various neural types. </summary>
[<MessagePackObject; StructuralEquality; StructuralComparison>]
type NeuralStructure =
    struct
        [<Key(0)>]
        val Groups : NeuralGroup list
        [<Key(1)>]
        val GroupSizes : int list
        [<Key(2)>]
        val GroupIndices : int list
        [<Key(3)>]
        val Size : int
        new(groups) = {Groups=groups; GroupSizes = [for g in groups do yield g.NeuronCount]; GroupIndices = List.scan (+) 0 [for g in groups do yield g.NeuronCount]; Size = List.sumBy (fun (g:NeuralGroup) -> g.NeuronCount) groups}
        new(groups, groupsizes, groupindices,size) = {Groups=groups; GroupSizes = groupsizes; GroupIndices = groupindices; Size = size}
    end



/// <summary> A set of relational symmetries to optimize the probability map for connecting to various clusters, structures, and types. These are the five possible relations for whether it is an internal or external connection. Uppercase indicates an internal relation, while lowercase represents an external relation. </summary>
type NeuralRelation =
| NR_CST 
| NR_cST 
| NR_cSt 
| NR_csT 
| NR_cst 
    
/// <summary> Sequential probability maps for forming synaptic connections between neurons. The first indicates the probability of each possible neural relation. Then of the selected neural relation, selects a structure, then a group within the structure, and finally a cluster within the group. </summary>
type ConnectivityMap = PartitionTree<decimal,NeuralRelation * PartitionTree<int,PartitionTree<int,PartitionTree<decimal,NeuralRelation * int * int * int>>>>

/// <summary> Passes instructional information to neurons. </summary>
[<MessagePackObject>]
type NeuralMessage =
| ConnectInput of NeuralAddress
| DisconnectInput of NeuralAddress
| Die

/// <summary> Unused. Aids the organization of the nervous system. Forms and breaks neural connections between neurons, and passes neurons between the charge of various glia. In previous incarnations these provided a structure for neurons to grow on. </summary>
[<MessagePackObject>]
type Glia =
    struct
        [<Key(0)>]
        val WatchedRequired : NeuralAddress list
        [<Key(1)>]
        val WatchedOptional : NeuralAddress list
        [<Key(3)>]
        val MaxSize : int
        [<Key(4)>]
        val NeuralMap : ConnectivityMap
        new(watchedrequired,watchedoptional, maxsize, neuralmap) = {WatchedRequired = watchedrequired; WatchedOptional = watchedoptional; MaxSize = maxsize; NeuralMap = neuralmap}
    end

// These version of the dendrites would be particularly useful if the neurons were design to only pass messages upon sufficient activation rather than every time. Has the potential to increase the processing efficiency, with much greater gains for larger nervous systems.    
/// <summary> Unused. The inputs of a neuron. Designed for synaptic message passing to be handled by the pre-synaptic neuron rather than the post-synaptic neuron. </summary>
[<MessagePackObject>]
type Dendrites(queue, senders) =
    [<Key(0)>]
    let queue = queue
    new() = Dendrites(ConcurrentQueue<float>(),ConcurrentBag<NeuralAddress>())
    [<Key(1)>]
    member this.Senders = senders
    member this.SendValue = queue.Enqueue
    member this.RecieveValue = queue.TryDequeue
           
// Some of the values are mutable to minimize the need for garbage collection via the use of a pair of arrays acting as swappable read and write buffers.
/// <summary> A single neuron. Contains current information for learned information, output, inputs. It also contains some convenient information to prioritize processing efficiency over memory efficiency.
[<MessagePackObject>]
type Neuron =
        [<Key(0)>]
        val NeuralType : NeuralType
        [<Key(1)>]
        val mutable Dendrites : (NeuralAddress * bool * float ref) list
        [<Key(2)>]
        val mutable Axon : float
        [<Key(3)>]
        val MessageQueue : ConcurrentQueue<NeuralMessage>
        [<Key(4)>]
        val CurrentAccumRate : float
        [<Key(5)>]
        val ExpectedAccumRate : float
        [<Key(6)>]
        val mutable CurrentAccum : float
        [<Key(7)>]
        val mutable ExpectedAccum : float
        [<Key(8)>]
        val mutable Variance : float
        new(neuraltype, dendrites, axon, currentaccumrate, expectedaccumrate, currentaccum, expectedaccum, variance) = {NeuralType = neuraltype; Dendrites = dendrites; MessageQueue = ConcurrentQueue(); Axon = axon; CurrentAccumRate = currentaccumrate; ExpectedAccumRate = expectedaccumrate; CurrentAccum = currentaccum; ExpectedAccum = expectedaccum; Variance = variance}
        new(neuraltype, dendrites, axon, messagequeue, currentaccumrate, expectedaccumrate, currentaccum, expectedaccum, variance) = {NeuralType = neuraltype; Dendrites = dendrites; MessageQueue = messagequeue; Axon = axon; CurrentAccumRate = currentaccumrate; ExpectedAccumRate = expectedaccumrate; CurrentAccum = currentaccum; ExpectedAccum = expectedaccum; Variance = variance}
        
/// <summary> A nervous system contains neural structures, senses, and muscles. </summary>
[<MessagePackObject; StructuralEquality; StructuralComparison>]
type NervousSystem =
    struct
        [<Key(0)>]
        val Structures : NeuralStructure list
        [<Key(1)>]
        val StructureSizes : int list
        [<Key(2)>]
        val StructureIndices : int list
        [<Key(3)>]
        val Size : int
        [<Key(4)>]
        val Senses : NeuralAddress list // The inputs of the nervous system.
        [<Key(5)>]
        val Muscles : NeuralAddress list // The outputs of the nervous system.
        new(structures, senses, muscles) = {Structures=structures; StructureSizes = [for s in structures do yield s.Size]; StructureIndices = List.scan (+) 0 [for s in structures do yield s.Size]; Size = List.sumBy (fun (s:NeuralStructure) -> s.Size) structures; Muscles = muscles; Senses = senses}
        new(structures, structuresizes, structureindices, size, senses, muscles) = {Structures=structures; StructureSizes = structuresizes; StructureIndices = structureindices; Size = size; Muscles = muscles; Senses = senses}
        member this.NeuralTypeOf (neuraladdress : NeuralAddress) = this.Structures.[neuraladdress.StructureIndex].Groups.[neuraladdress.GroupIndex].NeuralType
        member this.GetStructureByNeuralAddress (address : NeuralAddress) = this.Structures.[address.StructureIndex]
        member this.GetGroupByNeuralAddress (address : NeuralAddress) = this.Structures.[address.StructureIndex].Groups.[address.GroupIndex]
        member this.GetClusterByNeuralAddress (address : NeuralAddress) = this.Structures.[address.StructureIndex].Groups.[address.GroupIndex].Clusters.[address.ClusterIndex]
    end

/// <summary> Creates a neural structure from a list of the number of groups for each neural type. </summary>
let CreateNeuralStructureFromTypes (structureindex : int) (ntl : (int * NeuralType) list) =
    NeuralStructure( // Create a structure.
        [for g in [0..ntl.Length-1] do  // Create the list of groups the structure contains.
            let size,nt = ntl.[g]
            yield NeuralGroup( // Create the individual group.
                [for c in [0..size-1] do // Create the clusters of the group.
                    yield NeuralCluster( // Create the individual cluster.
                        [for n in [0..nt.ClusterSize-1] do // The neural addresses in the cluster.
                            yield NeuralAddress(structureindex, g, c, n)])],
                            nt)] // The neural type of the cluster.
    )
    
/// <summary> Creates a nervous system from neural structures, senses, and muscles. </summary>
let CreateNervousSystemFromStructures (ntll : (int * NeuralType) list list) (senses) (muscles) =
    NervousSystem(
        [for s in [0 .. ntll.Length-1] do yield CreateNeuralStructureFromTypes s ntll.[s]],
        senses,
        muscles
    )

/// <summary> Unused. A message passed to neurons. </summary>
type NeuralMessage<'address> =
| Fire of float
| ConnectTo of 'address
| DisconnectFrom of 'address

/// <summary> Returns true if the neural relation is a intra-cluster relation, or false if it is a inter-cluster relation. </summary>
let getnrc r = match r with | NR_CST -> true | _ -> false
/// <summary> Returns true if the neural relation is a intra-structure relation, or false if it is a inter-structure relation. </summary>
let getnrs r = match r with | NR_CST -> true | NR_cST -> true | NR_cSt -> true | _ -> false
/// <summary> Returns true if the neural relation is a intra-type relation, or false if it is a inter-type relation. </summary>
let getnrt r = match r with | NR_CST -> true | NR_cST -> true | NR_csT -> true | _ -> false

/// <summary> Creates a probability map to form synaptic connections between neurons for a given nervous system. </summary>
let CreateConnectivityMap (ns : NervousSystem) : ConnectivityMap =
    /// <summary> Gets a factor for the probability weight for a synaptic connection, relative to a given neural relation. </summary>
    let getbalance nr (nt:NeuralType) =
        let flipper f v =
            if f then (1.0-v) else v
        (flipper (getnrc nr) nt.IntraInterBalanceCluster) * // The cluster relation factor.
        (flipper (getnrs nr) nt.IntraInterBalanceStructure) * // The structure relation factor.
        (flipper (getnrt nr) nt.IntraInterBalanceGroup) // The group/type relation factor.
    
    /// <summary> Gets the probability weight for a particular possible synaptic connection.
    let mapprobability nr ((size,neuraltype),(structure,group,cluster)) =
        (decimal size * decimal (getbalance nr neuraltype),(structure,group,cluster))
    
    /// <summary> A list of the probability weights for each possible neural relation. </summary>
    let nrsizes : Map<NeuralRelation,_> =
        Map.ofList
            (List.map
                (fun nr -> // For each neural relation:
                    (nr, // Use the neural relation as a key.
                        List.fold // Find the total probability of the neural relation by summing the probabilities of all possible synaptic connections in the nervous system when assuming that neural relation holds.
                            (fun s i -> fst( i))
                            (decimal 0)
                            [for s in ns.Structures do 
                                yield! [for g in s.Groups do 
                                        yield! [for c in g.Clusters do 
                                                yield! (
                                                        let p = mapprobability nr (((decimal)c.Neurons.Length, g.NeuralType),(s,g,c))
                                                        if fst p <> decimal 0 then
                                                            [p]
                                                        else
                                                            []
                                                    )]]]))
                
                [NR_CST;NR_cST;NR_cSt;NR_csT;NR_cst])
    /// <summary> A list the indices to be used as the basis for the probability map. </summary>
    let nrindices =
        Map.ofList
            (List.scan
                (fun (_,accum) (nr,size) -> (nr,accum+size))
                (NR_CST,decimal 0)
                (Map.toList nrsizes)
            )
    //let minimals =
    //    List.fold (fun s (r,w) -> if w > decimal 0 && w < s then w else s) System.Decimal.MaxValue 
    
    let listlast (l : _ list) = l.[l.Length-1]
    let duplicatelastitem (f : _ -> _) (l : (_ * _) list) = (l |> listlast |> fst |> f,l |> listlast |> snd) :: l

    AddItemsHigh // Begin the creation of the partition map that will be returned to use as a probability map.
        [for nr in [NR_CST;NR_cST;NR_cSt;NR_csT;NR_cst] do // Make maps for each possible relation.
            yield (
                nrindices.[nr], // The border separating the partitions. 
                (
                    nr, // The relation selected.
                    AddItemsHigh // Create the probability map for the selected relation.
                        ([for si in [0 .. ns.Structures.Length - 1] do // The possible structures to be selected.
                            yield (
                                ns.StructureIndices.[si], // The partition border based solely on the number of neurons in each structure.
                                let s = ns.Structures.[si]
                                AddItemsHigh // Create the probability map for the selected structure.
                                    ([for gi in [0 .. s.Groups.Length - 1] do // The possible groups.
                                        yield (
                                            s.GroupIndices.[gi], // The partition border based solely on the number of neurons in each group.
                                            let g = s.Groups.[gi]
                                            let mp (c:NeuralCluster) = fst (mapprobability nr ((decimal c.Neurons.Length,g.NeuralType),(s,g,c))) // Gets the probability of a cluster based on the relation.
                                            
                                            AddItemsHigh // Create the probability map for the selected group.
                                                (List.scan // Converts weights to borders.
                                                    (fun (n1,c1) (n2,c2) -> (n1+n2,c2)) // Progressively sums weights to create border positions.
                                                    (decimal 0,(NR_CST,si,gi,0))
                                                    (
                                                        (decimal 0, (nr,si,gi,0)) ::
                                                        [for ci in [1 .. g.Clusters.Length - 1] do // The possible clusters.
                                                            let c = g.Clusters.[ci-1]
                                                            yield (
                                                                mp c, // The weight of the cluster.
                                                                (nr,si,gi,ci) // The probability maps ultimately return a neural relation and partial neural address to uniquely specify a particular cluster.
                                                            )
                                                        ] |> duplicatelastitem (fun d -> d + (g.Clusters |> listlast |> mp))
                                                    )
                                                )
                                                (Item((NR_CST,0,0,0)))
                                            |> Balance |> PartitionTree
                                        )
                                    ] |> duplicatelastitem (fun i -> i + listlast s.GroupSizes))
                                    (Item(PartitionTree(Item((NR_CST,0,0,0)))))
                                |> Balance |> PartitionTree
                            )
                        ] |> duplicatelastitem (fun i -> i + listlast ns.StructureSizes))
                        (Item(PartitionTree(Item(PartitionTree(Item((NR_CST,0,0,0)))))))
                    |> Balance |> PartitionTree
                )
            )
        ]
        (Item (NR_CST,PartitionTree(Item(PartitionTree(Item(PartitionTree(Item((NR_CST,0,0,0)))))))))
    |> Balance |> PartitionTree

/// <summary> Maps of the shortest routes to each sense from all path-connected neurons. </summary>
type SensoryMap = Map<NeuralAddress,(Map<int,NeuralAddress list> * Map<NeuralAddress,int>)>

/// <summary> Creates a probability map to form a connection from a neuron to any neuron which is already path-connected to at least one sense. </summary>
let CreateSensoryConnectionMap (cluster : NeuralAddress list) (sensoryadaptions : SensoryAdaption list) (sensorymaps : SensoryMap) = 
    
    let listlast (l : _ list) = l.[l.Length-1]
    let duplicatelastitem (f : _ -> _) (l : (_ * _) list) = (l |> listlast |> fst |> f,l |> listlast |> snd) :: l
    
    let getratio (smbd : Map<int,NeuralAddress list>, smbn : Map<NeuralAddress,int>) =
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
            
    

let SelectNeuron (rand : System.Random) (neuron : NeuralAddress) (ns : NervousSystem) (map : ConnectivityMap) (sensorymap : SensoryMap) : NeuralAddress =
    let defaultto0 option = match option with | Some value -> value | None -> 0
    let defaultto0d option = match option with | Some value -> value | None -> decimal 0.0
    
    let relation, structuresmap = map.[defaultto0d map.HighestKey * decimal(rand.NextDouble())];
    let groupsmap = structuresmap.[rand.Next(structuresmap.HighestKey|>defaultto0)]
    let clustersmap = groupsmap.[rand.Next(groupsmap.HighestKey|>defaultto0)]
    let nr, s, g, c = clustersmap.[defaultto0d clustersmap.HighestKey * decimal(rand.NextDouble())]
    let nt = ns.NeuralTypeOf neuron
    let rn () = NeuralAddress(s,g,c,rand.Next(ns.Structures.[s].Groups.[g].NeuralType.ClusterSize))
    if rand.NextDouble() < nt.SensoryAdaptionRate then
        let scm = CreateSensoryConnectionMap (ns.Structures.[s].Groups.[g].Clusters.[c].Neurons) nt.SensoryAdaptions sensorymap
        if scm.IsSome then
            scm.Value.[rand.NextDouble() * scm.Value.HighestKey.Value]
        else
            rn()
    else
        rn()


let CreateSensoryMusclePathConnection (rand : System.Random) (ns : NervousSystem) (cmap : ConnectivityMap) (sensorymap : SensoryMap) (neuraladdress : NeuralAddress) (neuron : Neuron) =
    let randvalue = rand.NextDouble()
    let rec connection index results =
        if index < neuron.NeuralType.AxonalSynapses - neuron.Dendrites.Length && (index < neuron.NeuralType.DendritricSynapsesMin - neuron.Dendrites.Length || randvalue < neuron.NeuralType.AdaptionRate * System.Math.Pow(0.5, float index)) then
            let selectedneuron =
                SelectNeuron
                    rand
                    neuraladdress
                    ns
                    cmap
                    sensorymap
            (
                selectedneuron
                ,(rand.NextDouble() < neuron.NeuralType.DirectInverseBalance)
                , ref 1.0
            ) 
            :: results
            |> connection (index + 1)
        else
            results
    connection 1 neuron.Dendrites
    
let CreateConnection (rand : System.Random) (ns : NervousSystem) (cmap : ConnectivityMap) (sensorymap : SensoryMap) (neuraladdress : NeuralAddress) (neuron : Neuron) =
    let randvalue = rand.NextDouble()
    let rec connection index results =
        if index < neuron.NeuralType.AxonalSynapses - neuron.Dendrites.Length && (index < neuron.NeuralType.DendritricSynapsesMin - neuron.Dendrites.Length || randvalue < neuron.NeuralType.AdaptionRate * System.Math.Pow(0.5, float index)) then
            let selectedneuron =
                SelectNeuron
                    rand
                    neuraladdress
                    ns
                    cmap
                    sensorymap
            (
                selectedneuron
                ,(rand.NextDouble() < neuron.NeuralType.DirectInverseBalance)
                , ref 1.0
            ) 
            :: results
            |> connection (index + 1)
        else
            results
    connection 1 neuron.Dendrites
    
    
                
        
            
let CreateConnections (rand : System.Random) (ns : NervousSystem) (neurons : Map<NeuralAddress,Neuron>) (cmap : ConnectivityMap) (sensorymap : SensoryMap) =
    Map.map 
        (fun (na: NeuralAddress) (n: Neuron) ->
            Neuron(
                n.NeuralType,
                (CreateConnection rand ns cmap sensorymap na n),
                n.Axon,
                n.CurrentAccumRate,
                n.ExpectedAccumRate,
                n.CurrentAccum,
                n.ExpectedAccum,
                n.Variance
            )
        )
        neurons

let SubProcessBalanceSynapses (n : Neuron) input (map : NeuralAddress -> Neuron) sum (count : int) =
    input
    |> List.iter 
        (fun (na,isinverse,mult) -> 
            let s = map(na)
            let axon = n.Axon
            let value = 
                if isinverse then
                    match map(na).NeuralType.NeuralType with
                    | NtOffset -> -axon * !mult
                    | NtScaling -> (if axon = 0.0 then 1.0 else 1.0 / axon) * !mult
                    | NtSpiking -> (1.0 - axon) * !mult
                else
                    axon * !mult
            if isinverse then
                mult :=
                    (s.ExpectedAccumRate * (max 0.0 <| axon)) * (max 1.0 <| (s.ExpectedAccum - (sum - value)) / value * !mult)
                    + (1.0 - s.ExpectedAccumRate * (max 0.0 <| axon)) * !mult
            else
                mult :=
                    (s.ExpectedAccumRate * (max 0.0 <| axon)) * (max 1.0 <| (s.ExpectedAccum - (sum - value)) / axon)
                    + (1.0 - s.ExpectedAccumRate * (max 0.0 <| axon)) * !mult
        )

let SubprocessLearn (n : Neuron) sum count =
    
    let inputsum,inputcount = sum, count
    let input = inputsum //   / float inputcount
    let currentaccum = input * n.CurrentAccumRate + (1.0-n.CurrentAccumRate) * n.CurrentAccum
    let expectedaccum = input * n.ExpectedAccumRate + (1.0-n.ExpectedAccumRate) * n.ExpectedAccum
    let expectedvar = abs(input - expectedaccum) * n.ExpectedAccumRate + (1.0-n.ExpectedAccumRate) * n.Variance
    match n.NeuralType.NeuralType with
    | NtScaling ->
        let signal = if currentaccum = expectedaccum then 1.0 else if expectedaccum = 0.0 then currentaccum else currentaccum / expectedaccum
        (
            currentaccum,
            expectedaccum,
            expectedvar,
            System.Math.Pow(signal, 1.0 / expectedvar)
        )
    | NtOffset -> 
        let signal = 1.0 + currentaccum - expectedaccum
        (
            currentaccum,
            expectedaccum,
            expectedvar,
            signal / expectedvar
        )
    | NtSpiking ->
        let signal = if currentaccum > expectedaccum then 2.0 else 1.0
        (
            currentaccum,
            expectedaccum,
            expectedvar,
            signal / expectedvar
        )

let SubprocessAdapt (map : NeuralAddress -> Neuron) (rand : System.Random) (neuron : Neuron) sum count =
    let nt = neuron.NeuralType
    let input = neuron.Dendrites
    let avg = sum / float count
    let worst = 
        List.sortByDescending
            (fun (na,isinverse,mult) ->
                abs(
                    if isinverse then
                        match map(na).NeuralType.NeuralType with
                        | NtOffset -> -map(na).Axon
                        | NtScaling -> if map(na).Axon = 0.0 then 1.0 else (1.0 / map(na).Axon)
                        | NtSpiking -> 1.0 - map(na).Axon
                    else
                        map(na).Axon
                ) 
                /
                (abs(neuron.Axon)+1.0)
            )
            input
        |> List.mapi (fun i (a,b,c)-> (i+1,a,b,c))
    (List.choose 
    (fun (i,na,isinverse,mult) ->
        if rand.NextDouble() < nt.AdaptionRate * System.Math.Pow(0.5,float i) then
            None
        else
            Some (na,isinverse,mult))
    worst)

let ProcessNeuron (neuraladdress : NeuralAddress) (nervoussytem : NervousSystem) (connectivitymap : ConnectivityMap) (sensorymap : SensoryMap) (map : NeuralAddress->Neuron) (rand : System.Random) (sense : float option) =
    let neuron = map neuraladdress
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
    
    SubProcessBalanceSynapses neuron input map sum count
    let newcurrentaccum,newexpectedaccum, newvariance, newoutput =
        SubprocessLearn neuron sum count
    let adaptedinputs =
        SubprocessAdapt map rand neuron sum count
    let newinputs =
        CreateConnection 
            rand 
            nervoussytem 
            connectivitymap 
            sensorymap
            neuraladdress 
            (Neuron(
                nt,
                adaptedinputs,
                newoutput,
                neuron.CurrentAccumRate,
                neuron.ExpectedAccumRate,
                newcurrentaccum,
                newexpectedaccum,
                newvariance))
    Neuron(
        nt,
        newinputs,
        newoutput,
        neuron.CurrentAccumRate,
        neuron.ExpectedAccumRate,
        newcurrentaccum,
        newexpectedaccum,
        newvariance)



let CreateIndexedNeurons (ns : NervousSystem) =
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
    ] |> Map.ofList

/// <summary>Processes one step of all neural activity. </summary>
/// <param name="nervoussytem">The Nervous System. </param>
/// <param name="connectivitymap">The probability map of particular synaptic connections forming.</param>
/// <param name="neuralmap">The map from neural addresses to the neurons to be processed.</param>
/// <param name="rand">The random number generator to be used.</param>
/// <param name="senses">The neural addresses of senses and their inputs.</param>
/// <param name="muscles">The neural addresses of muscles.</param>
/// <returns>The new neural map and the outputs of muscles.</returns>
let ProcessAll (nervoussytem : NervousSystem) (connectivitymap : ConnectivityMap) (rand : System.Random) (neuralmap : Map<NeuralAddress,Neuron>) (sensorymap : SensoryMap) (senses : Map<NeuralAddress,float>) (muscles : NeuralAddress list) = 
    neuralmap
    |> Map.map (fun (na:NeuralAddress) (n:Neuron) -> ProcessNeuron na nervoussytem connectivitymap sensorymap (fun na -> neuralmap.[na]) rand (if senses.ContainsKey(na) then Some(senses.[na]) else None))
        
let CreateSensoryMap  (nervoussytem : NervousSystem) (neuralmap : Map<NeuralAddress,Neuron>) (senses : NeuralAddress list) : SensoryMap =
    let nodes =
        neuralmap
        |> Map.map (fun (na : NeuralAddress) (n : Neuron) -> {location = na; data = n; adjacent = n.Dendrites |> List.unzip3 |> (fun (a,_,_) -> a)})
    let pathmaps =
        senses
        |> List.map (PathMap nodes)
    let depthsmaps =
        pathmaps
        |> List.map 
            (fun (_,nmbd) -> 
                nmbd |> Map.toList |> List.collect 
                    (fun (depth : int, nal : NeuralAddress list) -> nal |> List.map (fun na -> (na,depth)))
                |> Map.ofList
            )
    pathmaps
    |> List.unzip
    |> snd
    |> (fun a b -> (b,a)) depthsmaps
    ||> List.zip
    |> List.zip senses
    |> Map.ofList



let GetMuscleResults neuralmap muscles =
    neuralmap
    |> Map.filter (fun (na : NeuralAddress) (_) -> List.contains na muscles)
    |> Map.map (fun na (n : Neuron) -> n.Axon)

