namespace Soevnn.Tests

open NUnit.Framework
open Soevnn.Utilities.Partitions

[<TestFixture>]
type PartitionTreeTests () =

    [<SetUp>]
    member this.Setup () =
        ()
        

    [<Test>]
    [<Category("AddItemHigh")>]
    member this.TestAddItemHigh1 () =
        let initialvalue = Partition(2,Item "First", Item "Second")
        let value = AddItemHigh 3 "Third" initialvalue
        let expectedvalue = Partition(2, Item "First", Partition(3, Item "Second", Item "Third"))
        Assert.AreEqual(expectedvalue, value)
        
    [<Test>]
    [<Category("AddItemHigh")>]
    member this.TestAddItemHigh2 () =
        let initialvalue = Partition(2,Item "First", Item "Third")
        let value = AddItemHigh 1 "Second" initialvalue
        let expectedvalue = Partition(2, Partition(1,Item "First",  Item "Second"), Item "Third")
        Assert.AreEqual(expectedvalue, value)

    [<Test>]
    [<Category("AddItemLow")>]
    member this.TestAddItemLow1 () =
        let initialvalue = Partition(2,Item "First", Item "Third")
        let value = AddItemLow 3 "Second" initialvalue
        let expectedvalue = Partition(2, Item "First", Partition(3, Item "Second", Item "Third"))
        Assert.AreEqual(expectedvalue, value)

    [<Test>]
    [<Category("AddItemLow")>]
    member this.TestAddItemLow2 () =
        let initialvalue = Partition(2,Item "Second", Item "Third")
        let value = AddItemLow 1 "First" initialvalue
        let expectedvalue = Partition(2, Partition(1, Item "First", Item "Second"), Item "Third")
        Assert.AreEqual(expectedvalue, value)


    [<Test>]
    [<Category("GetItem")>]
    member this.TestGetItem1 () =
        let initialvalue = Partition(2, Partition(1, Item "First", Item "Second"), Item "Third")
        let value = GetItem 0 initialvalue
        let expectedvalue = "First"
        Assert.AreEqual(expectedvalue, value)
    
    [<Test>]
    [<Category("GetItem")>]
    member this.TestGetItem2 () =
        let initialvalue = Partition(2, Partition(1, Item "First", Item "Second"), Item "Third")
        let value = GetItem 2 initialvalue
        let expectedvalue = "Third"
        Assert.AreEqual(expectedvalue, value)
    
    [<Test>]
    [<Category("GetItem")>]
    member this.TestGetItem3 () =
        let initialvalue = Partition(2, Partition(1, Item "First", Item "Second"), Item "Third")
        let value = GetItem 5 initialvalue
        let expectedvalue = "Third"
        Assert.AreEqual(expectedvalue, value)
    
    [<Test>]
    [<Sequential>]
    [<Category("GetItem")>]
    [<Category("Long")>]
    member this.TestGetItem4 ([<Random(1,10000,40)>] seed : int, [<Range(25,1000,25)>] size : int, [<Range(22,100,2)>] accesscount : int) =
        let rand = System.Random(seed)
        let items = System.Collections.Generic.List()
        for i in [0..size-1] do
            items.Add((i,i.ToString())) |> ignore
        let rec createinitialvalue node =
            if items.Count <= 0 then
                node
            else 
                let chosen = rand.Next(0,items.Count)
                let (key,item) = items.[chosen] 
                items.RemoveAt(chosen)
                createinitialvalue (AddItemHigh key item node)
        let initialvalue = createinitialvalue (Item("Default"))
        for i in [0..accesscount-1] do
            let chosen = rand.Next(0,size-1)
            let value = GetItem (chosen) initialvalue
            let expectedvalue = chosen.ToString()
            Assert.AreEqual(expectedvalue, value)

            
    [<Test>]
    [<Sequential>]
    [<Category("GetIndex")>]
    [<Category("Long")>]
    member this.TestGetIndex1 ([<Random(1,10000,40)>] seed : int, [<Range(25,1000,25)>] size : int, [<Range(22,100,2)>] accesscount : int) =
        let rand = System.Random(seed)
        let items = System.Collections.Generic.List()
        for i in [0..size-1] do
            items.Add((i,i.ToString())) |> ignore
        let rec createinitialvalue node =
            if items.Count <= 0 then
                node
            else 
                let (key,item) = items.[0] 
                items.RemoveAt(0)
                createinitialvalue (AddItemHigh key item node)
        let initialvalue = createinitialvalue (Item("Default"))
        for i in [0..accesscount-1] do
            let chosen = rand.Next(0,size-1)
            let value = GetIndex (chosen) initialvalue
            let expectedvalue = chosen
            Assert.AreEqual(expectedvalue, value)
    
    
    [<Test>]
    [<Category("LiftLeft")>]
    member this.TestLiftLeft1 () =
        let initialvalue = Partition(3, Partition(2,Item "First", Item "Second"), Item "Third")
        let value = LiftLeft initialvalue
        let expectedvalue = Partition(2, Item "First", Partition(3, Item "Second", Item "Third"))
        Assert.AreEqual(expectedvalue, value)

    
    [<Test>]
    [<Category("LiftRight")>]
    member this.TestLiftRight1 () =
        let initialvalue = Partition(2, Item "First", Partition(3, Item "Second", Item "Third"))
        let value = LiftRight initialvalue
        let expectedvalue = Partition(3, Partition(2,Item "First", Item "Second"), Item "Third")
        Assert.AreEqual(expectedvalue, value)


    [<Test>]
    [<Category("LiftToRoot")>]
    member this.TestLiftToRoot1 () =
        let initialvalue = Partition(3, Partition(2,Item "First", Item "Second"), Item "Third")
        let value = LiftToRoot 0 initialvalue initialvalue
        let expectedvalue = Partition(2, Item "First", Partition(3, Item "Second", Item "Third"))
        Assert.AreEqual(expectedvalue, value)   
    
    [<Test>]
    [<Category("LiftToRoot")>]
    member this.TestLiftToRoot2 () =
        let initialvalue = Partition(2, Item "First", Partition(3, Item "Second", Item "Third"))
        let value = LiftToRoot 1 initialvalue initialvalue
        let expectedvalue = Partition(3, Partition(2,Item "First", Item "Second"), Item "Third")
        Assert.AreEqual(expectedvalue, value)  
   
    [<Test>]
    [<Sequential>]
    [<Category("Balance")>]
    [<Category("Long")>]
    member this.TestBalance1 ([<Random(1,10000,40)>] seed : int, [<Range(25,1000,25)>] size : int) =
        let rand = System.Random(seed)
        let items = System.Collections.Generic.List()
        for i in [0..size-1] do
            items.Add((i,i.ToString())) |> ignore
        let rec createinitialvalue node =
            if items.Count <= 0 then
                node
            else 
                let chosen = rand.Next(0,items.Count)
                let (key,item) = items.[chosen] 
                items.RemoveAt(chosen)
                createinitialvalue (AddItemHigh key item node)
        let initialvalue = Balance (createinitialvalue (Item("Default")))

        let bvalue = GetBalance(initialvalue)
        Assert.GreaterOrEqual(bvalue, -2)
        Assert.LessOrEqual(bvalue, -1)
        Assert.LessOrEqual(GetMaxDepth initialvalue - GetMinDepth initialvalue, 3)

    
    [<Test>]
    [<Sequential>]
    [<Category("Balance")>]
    [<Category("Long")>]
    member this.TestBalance2 ([<Random(1,10000,40)>] seed : int, [<Range(25,1000,25)>] size : int, [<Range(22,100,2)>] accesscount : int) =
        let rand = System.Random(seed)
        let items = System.Collections.Generic.List()
        for i in [0..size-1] do
            items.Add((i,i.ToString())) |> ignore
        let rec createinitialvalue node =
            if items.Count <= 0 then
                node
            else 
                let chosen = rand.Next(0,items.Count)
                let (key,item) = items.[chosen] 
                items.RemoveAt(chosen)
                createinitialvalue (AddItemHigh key item node)
        let initialvalue = Balance (createinitialvalue (Item("Default")))

        for i in [0..accesscount-1] do
            let chosen = rand.Next(0,size-1)
            let value = GetItem (chosen) initialvalue
            let expectedvalue = chosen.ToString()
            Assert.AreEqual(expectedvalue, value)

    
    [<Test>]
    [<Sequential>]
    [<Category("Balance")>]
    [<Category("Long")>]
    member this.TestBalance3 ([<Random(1,10000,40)>] seed : int, [<Range(25,1000,25)>] size : int, [<Range(22,100,2)>] accesscount : int) =
        let rand = System.Random(seed)
        let items = System.Collections.Generic.List()
        for i in [0..size-1] do
            items.Add((i,i.ToString())) |> ignore
        let rec createinitialvalue node =
            if items.Count <= 0 then
                node
            else 
                let chosen = rand.Next(0,items.Count)
                let (key,item) = items.[chosen] 
                items.RemoveAt(chosen)
                createinitialvalue (AddItemHigh key item node)
        let initialvalue = Balance (createinitialvalue (Item("Default")))

        for i in [0..accesscount-1] do
            let chosen = rand.Next(0,size-1)
            let value = GetIndex (chosen) initialvalue
            let expectedvalue = chosen
            Assert.AreEqual(expectedvalue, value)

