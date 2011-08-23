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
    let q = MinQueue ([1;2;3], id)
    Assert.That (q.Vector, Is.EqualTo [1;2;3])
  [<Test>]
  member this.MinQMakeHeapifiesCompletely () =
    let q = MinQueue ([13; 16; 14; 15; 1; 8; 7; 3; 9; 10], id)
    Assert.That (q.Vector, Is.EqualTo [1; 3; 7; 9; 10; 8; 14; 15; 13; 16])
  [<Test>]
  member this.MinQExtractMinReturnsMin () =
    let q = MinQueue ([13; 16; 14; 15; 1; 8; 7; 3; 9; 10], id)
    Assert.That (q.ExtractMin (), Is.EqualTo 1)
  [<Test>]
  member this.MinQExtractMinRemovesMin () =
    let q = MinQueue ([13; 16; 14; 15; 1; 8; 7; 3; 9; 10], id)
    q.ExtractMin () |> ignore
    Assert.That (q.Count, Is.EqualTo 9)
  [<Test>]
  member this.MinQExtractMinReheapifies () =
    let q = MinQueue ([13; 16; 14; 15; 1; 8; 7; 3; 9; 10], id)
    q.ExtractMin () |> ignore
    Assert.That (q.Vector, Is.EqualTo [3; 9; 7; 13; 10; 8; 14; 15; 16])
  [<Test>]
  member this.MinQHeapifiesBasedOnKey () =
    let q = MinQueue (['m'; 'p'; 'n'; 'o'; 'a'; 'h'; 'g'; 'c'; 'i'; 'j'], (fun c -> int c - 96))
    Assert.That (q.Vector, Is.EqualTo ['a';'c';'g';'i';'j';'h';'n';'o';'m';'p'])
  [<Test>]
  member this.MinQPrimHeapifiesBasedOnKey () =
    let q = MinQueuePrim (['m'; 'p'; 'n'; 'o'; 'a'; 'h'; 'g'; 'c'; 'i'; 'j'], (fun c -> int c - 96))
    Assert.That (q.Vector |> Seq.map (fun p -> p.vertex), Is.EqualTo ['a';'c';'g';'i';'j';'h';'n';'o';'m';'p'])
  [<Test>]
  member this.MinQPrimDecreasesKeyCorrectly () =
    let q = MinQueuePrim (['m'; 'p'; 'n'; 'o'; 'a'; 'h'; 'g'; 'c'; 'i'; 'j'], (fun c -> int c - 96))
    q.DecreaseKey {vertex='m';key=13;i=8} 2
    Assert.That (q.Vector |> Seq.map (fun p -> p.vertex), Is.EqualTo ['a';'m';'g';'c';'j';'h';'n';'o';'i';'p'])

