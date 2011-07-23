module MinQueueTest
open NUnit.Framework
open System.Collections.Generic
open Types
open Util
open MinQueue

[<TestFixture>]
type public MinQTester () =
  [<Test>]
  member this.MinQHeapifiesOnConstruction () =
    let q = MinQueue [1;2;3]
    Assert.That (q.Vector, Is.EqualTo [1;2;3])
  [<Test>]
  member this.MinQMakeHeapifiesCompletely () =
    let q = MinQueue [13; 16; 14; 15; 1; 8; 7; 3; 9; 10]
    Assert.That (q.Vector, Is.EqualTo [1; 3; 7; 9; 10; 8; 14; 15; 13; 16])
  [<Test>]
  member this.MinQExtractMinReturnsMin () =
    let q = MinQueue [13; 16; 14; 15; 1; 8; 7; 3; 9; 10]
    Assert.That (q.ExtractMin (), Is.EqualTo 1)
  [<Test>]
  member this.MinQExtractMinRemovesMin () =
    let q = MinQueue [13; 16; 14; 15; 1; 8; 7; 3; 9; 10]
    q.ExtractMin () |> ignore
    Assert.That (q.Count, Is.EqualTo 9)
  [<Test>]
  member this.MinQExtractMinReheapifies () =
    let q = MinQueue [13; 16; 14; 15; 1; 8; 7; 3; 9; 10]
    q.ExtractMin () |> ignore
    Assert.That (q.Vector, Is.EqualTo [3; 9; 7; 13; 10; 8; 14; 15; 16])
