namespace Soevnn.Tests

open NUnit.Framework
open Soevnn.Core

[<TestFixture>]
type NervousSystemTests () =

    [<SetUp>]
    member this.Setup () =
        ()
        

    
    [<Test>]
    [<Sequential>]
    [<Category("CreateNervousSystem")>]
    [<Category("Long")>]
    member this.CreateNS1 ([<Range(1,5,1)>] clustercount : int, [<Range(40,8,-8)>] clustersize : int) =
        let ns = CreateNervousSystemFromStructures [[(clustercount, NeuralType(0.0,0.0,0.0,0.0,12,12,12,0.01,0.5,0.05,clustersize,NtOffset,NpCluster,0.0,[]))]] [] []
        
        Assert.AreEqual(ns.Structures.[0].Groups.[0].Clusters.[0].NeuronCount,ns.Structures.[0].Groups.[0].Clusters.[0].Neurons.Length)
        Assert.AreEqual(ns.Structures.[0].Groups.[0].NeuronCount,ns.Structures.[0].Groups.[0].Clusters.Length*ns.Structures.[0].Groups.[0].NeuralType.ClusterSize)

        
        
    [<Test>]
    [<Category("LearnValue")>]
    [<Category("Long")>]
    member this.LearnValue1 () =
        let sense = NeuralAddress(0,0,0,0)
        let muscle = NeuralAddress(0,0,0,0)
        let rand = System.Random(100)
        let nt = NeuralType(0.0,0.0,0.0,0.0,0,0,0,0.01,0.5,0.05,1,NtScaling,NpCluster,0.0,[])
        let ns = CreateNervousSystemFromStructures [[(1, nt)]] [sense] [muscle]
        let cmap = CreateConnectivityMap ns
        let initialneurons : Map<NeuralAddress,Neuron> = 
            CreateConnections rand ns (CreateIndexedNeurons ns) (cmap) Map.empty

        let finalneurons =
            let rec processall inputfunction iteration maxiteration neurons =
                if iteration < maxiteration - 1 then
                    processall 
                        inputfunction 
                        (iteration + 1)
                        maxiteration
                        (ProcessAll ns cmap rand neurons Map.empty (Map.ofList [(sense, inputfunction iteration)]) [(muscle)])
                else
                    (ProcessAll ns cmap rand neurons Map.empty (Map.ofList [(sense, inputfunction iteration)]) [(muscle)])
            processall (fun i -> 100.0) 0 100 initialneurons

        let result =
            (GetMuscleResults finalneurons [muscle]).[muscle]

        Assert.Less(result, 1.05)
        Assert.Greater(result, 1.0)
        
        
        
    