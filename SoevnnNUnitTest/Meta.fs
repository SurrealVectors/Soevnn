namespace Soevnn.Tests


open NUnit.Framework
open System


type MTestClass1() =
    member this.Message = "Hello World!"


type MTestClassGeneric1<'T>() =
    member this.Message = "Hello World!"

[<TestFixture>]
type MetaTests () =
    
    
        [<SetUp>]
        member this.Setup () =
            ()
        
        [<Test>]
        [<Category("Meta")>]
        member this.Activator1 () =
            let t = typeof<MTestClass1>
            let mtc1i = Activator.CreateInstance(t)
            if mtc1i.GetType() = t then
                Assert.Pass()
            else
                Assert.Fail("Wrong type.")

        
        [<Test>]
        [<Category("Meta")>]
        member this.ActivatorG1 () =
            let t = typeof<MTestClassGeneric1<int>>
            let mtcg1i = Activator.CreateInstance(t)
            if mtcg1i.GetType() = t then
                Assert.Pass()
            else
                Assert.Fail("Wrong type.")