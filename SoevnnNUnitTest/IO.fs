namespace Soevnn.Tests

open NUnit.Framework
open Soevnn.Core
open Soevnn.Parallel.Core
open Soevnn.Serialization
open System.IO
open MessagePack


module TestModule =

    [<MessagePackObject>]
    type ModuleClassA(a : int) =
        [<Key(0)>]
        member _.A = a

[<MessagePackObject>]
type ClassA(a : int) =
    [<Key(0)>]
    member _.A = a

[<MessagePackObject>]
type TestUnion0 =
    | Tu0A
    | Tu0B of int

[<MessagePackObject>]
type TestUnion1 =
    | Tu1A of unit
    | Tu1B of int
    
[<MessagePackObject>]
type TestUnion2 =
    | Tu2A
    | Tu2B

[<TestFixture>]
type IOTests () =
    let neuronequality (a : Neuron) (b : Neuron) =
        (a.NeuralType, a.Dendrites, a.Axon, a.MessageQueue.ToArray(), a.CurrentAccumRate, a.ExpectedAccumRate, a.CurrentAccum, a.ExpectedAccum, a.Variance) =
            (b.NeuralType, b.Dendrites, b.Axon, b.MessageQueue.ToArray(), b.CurrentAccumRate, b.ExpectedAccumRate, b.CurrentAccum, b.ExpectedAccum, b.Variance)
    


    [<SetUp>]
    member this.Setup () =
        ()
    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadRef () =
        let r = ref 1
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream,r,MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slr = 
            try
                Some <|MessagePackSerializer.Deserialize<int ref>(memstream,MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slr with
        | Some(slr) -> if slr.Value = r.Value then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadTuple () =
        let tup = (1,"2")
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream,tup)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let sltup = 
            try
                Some <|MessagePackSerializer.Deserialize<int * string>(memstream)
            with
                | :? MessagePackSerializationException as ex -> None
        match sltup with
        | Some(sltup) -> if sltup = tup then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadModuleClassA () =
        let mc = new TestModule.ModuleClassA(1)
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream,mc)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let sltmc = 
            try
                Some <|MessagePackSerializer.Deserialize<TestModule.ModuleClassA>(memstream)
            with
                | :? MessagePackSerializationException as ex -> None
        match sltmc with
        | Some(sltmc) -> if sltmc.A = mc.A then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")
    
    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadClassA () =
        let c = new ClassA(1)
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream,c)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slc = 
            try
                Some <|MessagePackSerializer.Deserialize<ClassA>(memstream)
            with
                | :? MessagePackSerializationException as ex -> None
        match slc with
        | Some(slc) -> if slc.A = c.A then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadUnionTest0A () =
        let tu = Tu0A
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, tu, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let sltu = 
            try
                Some <|MessagePackSerializer.Deserialize<TestUnion0>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match sltu with
        | Some(sltu) -> if sltu = tu then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")
    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadUnionTest1A () =
        let tu = Tu1A()
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, tu, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let sltu = 
            try
                Some <|MessagePackSerializer.Deserialize<TestUnion1>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match sltu with
        | Some(sltu) -> if sltu = tu then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadUnionTest2A () =
        let tu = Tu2A
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, tu, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let sltu = 
            try
                Some <|MessagePackSerializer.Deserialize<TestUnion2>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match sltu with
        | Some(sltu) -> if sltu = tu then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")
        
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadUnionTest2B () =
        let tu = Tu2B
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, tu, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let sltu = 
            try
                Some <|MessagePackSerializer.Deserialize<TestUnion2>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match sltu with
        | Some(sltu) -> if sltu = tu then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadUnionTest2List () =
        let tu = [Tu2A; Tu2B]
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, tu, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let sltu = 
            try
                Some <|MessagePackSerializer.Deserialize<TestUnion2 list>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match sltu with
        | Some(sltu) -> if sltu = tu then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNeuralTypeMethod () =
        let nt = NtOffset
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, nt, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slnt = 
            try
                Some <|MessagePackSerializer.Deserialize<NeuralTypeMethod>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slnt with
        | Some(slnt) -> if slnt = nt then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNeuralAddress () =
        let na = new NeuralAddress(1,2,3,4)
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, na, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slna = 
            try
                Some <|MessagePackSerializer.Deserialize(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slna with
        | Some(slna) -> if slna = na then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadSensoryAdaption () =
        let sa = new SensoryAdaption(new NeuralAddress(1,2,3,4),0.5,0.5)
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, sa, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slsa = 
            try
                Some <|MessagePackSerializer.Deserialize<SensoryAdaption>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slsa with
        | Some(slsa) -> 
            if slsa = sa then 
                Assert.Pass() 
            else 
                Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNeuralType () =
        let nt = new NeuralType(0.0,0.0,0.0,0.0,1,1,1,0.0,0.0,0.0,1,NtOffset,NpCluster,0.0,[])
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize<NeuralType>(memstream, nt, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slnt = 
            try
                Some <|MessagePackSerializer.Deserialize<NeuralType>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slnt with
        | Some(slnt) -> if slnt = nt then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNeuron () =
        let nt = new NeuralType(0.0,0.0,0.0,0.0,1,1,1,0.0,0.0,0.0,1,NtOffset,NpCluster,0.0,[])
        let neuron = new Neuron(nt,[],0.0,0.0,0.0,0.0,0.0,0.0)
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, neuron, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slneuron = 
            try
                Some <|MessagePackSerializer.Deserialize<Neuron>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slneuron with
        | Some(slneuron) -> 
            if 
                neuronequality slneuron neuron
            then 
                Assert.Pass() 
            else 
                Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")
    
    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNeurons () =
        let nt = new NeuralType(0.0,0.0,0.0,0.0,1,1,1,0.0,0.0,0.0,1,NtOffset,NpCluster,0.0,[])
        let neurons = [|(NeuralAddress(1,2,3,4),new Neuron(nt,[],0.0,0.0,0.0,0.0,0.0,0.0))|]
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, neurons, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slneurons = 
            try
                Some <|MessagePackSerializer.Deserialize<array<NeuralAddress*Neuron>>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slneurons with
        | Some(slneurons) -> 
            if 
                Array.forall2 (fun (na1,n1) (na2,n2) -> na1 = na2 && neuronequality n1 n2) slneurons neurons
            then 
                Assert.Pass() 
            else 
                Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNeuralCluster () =
        let nc = new NeuralCluster([new NeuralAddress(0,0,0,0)])
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize<NeuralCluster>(memstream, nc, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slnc = 
            try
                Some <|MessagePackSerializer.Deserialize<NeuralCluster>(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slnc with
        | Some(slnc) -> if slnc = nc then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNS1 () =
        let ns = CreateNervousSystemFromStructures [[(3, NeuralType(0.0,0.0,0.0,0.0,12,12,12,0.01,0.5,0.05,3,NtOffset,NpCluster,0.0,[]))]] [] []
        let neurons = CreateIndexedNeuronsParallel ns

        use memstream = new MemoryStream()
        try
            SaveNervousSystem ns neurons memstream
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        
        let result =
            try
                let slns = LoadNervousSystem(memstream)
                match slns with
                | Some(loadedns,loadedneurons) -> 
                    if loadedns = ns && 
                        Seq.forall2 
                            (fun (na1,n1) (na2,n2) -> na1 = na2 && neuronequality n1 n2) 
                            neurons 
                            loadedneurons 
                    then
                        None
                    else
                        Some("Loaded not equal.")
                | None -> Some("Load failed.")
            with
                | exn -> Some("Load error:   " + exn.Message)
        
        match result with
        | None -> Assert.Pass()
        | Some(msg) -> Assert.Fail(msg)
        
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNS2 () =
        let ns = CreateNervousSystemFromStructures [[(3, NeuralType(0.0,0.0,0.0,0.0,12,12,12,0.01,0.5,0.05,3,NtOffset,NpCluster,0.0,[]))]] [] []
        let neurons = CreateIndexedNeuronsParallel ns

        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, ns, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
            
        let result =
            try
                let slns : NervousSystem option = MessagePackSerializer.Deserialize(memstream,MessagePackOptions)
                match slns with
                | Some(loadedns) -> 
                    if loadedns = ns then
                        None
                    else
                        Some("Loaded not equal.")
                | None -> Some("Load failed.")
            with
                | exn -> Some("Load error:   " + exn.Message)
            
        match result with
        | None -> Assert.Pass()
        | Some(msg) -> Assert.Fail(msg)
        
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNA () =
        let ns = CreateNervousSystemFromStructures [[(3, NeuralType(0.0,0.0,0.0,0.0,12,12,12,0.01,0.5,0.05,3,NtOffset,NpCluster,0.0,[]))]] [] []
        let neurons = CreateIndexedNeuronsParallel ns

        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, neurons, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
            
        let result =
            try
                let slns : _ option = MessagePackSerializer.Deserialize(memstream,MessagePackOptions)
                match slns with
                | Some(loadedneurons) -> 
                    if Seq.forall2 
                        (fun (na1,n1) (na2,n2) -> na1 = na2 && neuronequality n1 n2) 
                        neurons 
                        loadedneurons 
                    then
                        None
                    else
                        Some("Loaded not equal.")
                | None -> Some("Load failed.")
            with
                | exn -> Some("Load error:   " + exn.Message)
            
        match result with
        | None -> Assert.Pass()
        | Some(msg) -> Assert.Fail(msg)
    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadSequential () =
        let first = [new NeuralAddress(0,0,0,0)]
        let second = new NeuralCluster([new NeuralAddress(0,0,0,0)])
        use memstream = new MemoryStream()
        try
            MessagePackSerializer.Serialize(memstream, first, MessagePackOptions)
            MessagePackSerializer.Serialize(memstream, second, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        memstream.Position <- int64 0
        let slfirst = 
            try
                Some <|MessagePackSerializer.Deserialize(memstream, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slfirst with
        | Some(slfirst) -> 
            if slfirst = first then 
                let slsecond = 
                    try
                        Some <|MessagePackSerializer.Deserialize(memstream, MessagePackOptions)
                    with
                        | :? MessagePackSerializationException as ex -> None
                match slsecond with
                | Some(slsecond) -> 
                    if slsecond = second then 
                        Assert.Pass() 
                    else 
                        Assert.Fail("Second value mismatch. Loaded data does not equal original.")
                | _ -> Assert.Fail("Second load failed.")
            else 
                Assert.Fail("First value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("First load failed.")

    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadFile () =
        let nt = NtOffset
        use file = File.Open("saveloadfiletest.txt",FileMode.OpenOrCreate, FileAccess.ReadWrite) :> Stream
        try
            MessagePackSerializer.Serialize(file, nt, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        file.Position <- int64 0
        let slnt = 
            try
                Some <|MessagePackSerializer.Deserialize<NeuralTypeMethod>(file, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        file.Close()
        match slnt with
        | Some(slnt) -> if slnt = nt then Assert.Pass() else Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> Assert.Fail("Load failed.")


    
    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadFileSequential () =
        let first = [new NeuralAddress(0,0,0,0)]
        let second = new NeuralCluster([new NeuralAddress(0,0,0,0)])
        use file = File.Open("saveloadfiletest.txt",FileMode.OpenOrCreate, FileAccess.ReadWrite) :> Stream
        try
            MessagePackSerializer.Serialize(file, first, MessagePackOptions)
            MessagePackSerializer.Serialize(file, second, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        file.Position <- int64 0
        let slfirst = 
            try
                Some <|MessagePackSerializer.Deserialize(file, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slfirst with
        | Some(slfirst) ->
            if slfirst = first then
                let slsecond = 
                    try
                        Some <|MessagePackSerializer.Deserialize(file, MessagePackOptions)
                    with
                        | :? MessagePackSerializationException as ex -> None
                file.Close()
                match slsecond with
                | Some(slsecond) -> 
                    if slsecond = second then 
                        Assert.Pass() 
                    else 
                        Assert.Fail("Second value mismatch. Loaded data does not equal original.")
                | _ -> Assert.Fail("Second load failed.")
            else 
                file.Close()
                Assert.Fail("First value mismatch. Loaded data does not equal original.")
        | _ -> 
            file.Close()
            Assert.Fail("First load failed.")


    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNSNASequential () =
        
        let ns = CreateNervousSystemFromStructures [[(3, NeuralType(0.1,0.2,0.3,0.4,5,6,7,0.8,0.9,0.01,2,NtOffset,NpCluster,0.11,[]))]] [] []
        let neurons = CreateIndexedNeuronsParallel ns
        use file = File.Open("saveloadfiletest.txt",FileMode.OpenOrCreate, FileAccess.ReadWrite) :> Stream
        try
            MessagePackSerializer.Serialize(file, ns, MessagePackOptions)
            MessagePackSerializer.Serialize(file, neurons, MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        file.Position <- int64 0
        let slfirst = 
            try
                Some <|MessagePackSerializer.Deserialize(file, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match slfirst with
        | Some(slfirst) ->
            if slfirst = ns then
                let slsecond = 
                    try
                        Some <|MessagePackSerializer.Deserialize(file, MessagePackOptions)
                    with
                        | :? MessagePackSerializationException as ex -> None
                file.Close()
                match slsecond with
                | Some(slsecond) -> 
                    if Seq.forall2 
                        (fun (na1,n1) (na2,n2) -> na1 = na2 && neuronequality n1 n2) 
                        neurons 
                        slsecond 
                    then 
                        Assert.Pass() 
                    else 
                        Assert.Fail("Second value mismatch. Loaded data does not equal original.")
                | _ -> Assert.Fail("Second load failed.")
            else 
                file.Close()
                Assert.Fail("First value mismatch. Loaded data does not equal original.")
        | _ -> 
            file.Close()
            Assert.Fail("First load failed.")

    [<Test>]
    [<Category("IO")>]
    member this.SaveLoadNSNATuple () =
        
        let ns = CreateNervousSystemFromStructures [[(3, NeuralType(0.1,0.2,0.3,0.4,5,6,7,0.8,0.9,0.01,2,NtOffset,NpCluster,0.11,[]))]] [] []
        let neurons = CreateIndexedNeuronsParallel ns
        use file = File.Open("saveloadfiletest.txt",FileMode.OpenOrCreate, FileAccess.ReadWrite) :> Stream
        try
            MessagePackSerializer.Serialize(file, (ns,neurons), MessagePackOptions)
        with
            | exn -> Assert.Fail("Failed to save.   " + exn.Message)
        file.Position <- int64 0
        let sl = 
            try
                Some <|MessagePackSerializer.Deserialize(file, MessagePackOptions)
            with
                | :? MessagePackSerializationException as ex -> None
        match sl with
        | Some(slfirst,slsecond) ->
            if slfirst = ns &&
                Seq.forall2 
                    (fun (na1,n1) (na2,n2) -> na1 = na2 && neuronequality n1 n2) 
                    neurons 
                    slsecond 
            then
                Assert.Pass()
            else 
                file.Close()
                Assert.Fail("Value mismatch. Loaded data does not equal original.")
        | _ -> 
            file.Close()
            Assert.Fail("Load failed.")