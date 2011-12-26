module AllPairsShortestPathTest
open NUnit.Framework
open System.Collections.Generic
open Types
open Basic
open Util

let AssertGraphWeightEquals (g : Graph<'k,_,int>) (g' : Graph<'k,_,int>) =
  for u in g.Keys do
    for v,v' in Seq.zip g.[u].edges g'.[u].edges do
        Assert.That(v.meta, Is.EqualTo v'.meta)

[<TestFixture>]
type public AllPairsShortestPathTester () =
  let makeGraph () = 
    Dictionary.fromList [
      wvertex '1' [('2',3);('3',8);('5',-4)]
      wvertex '2' [('4',1);('5',7)]
      wvertex '3' [('2',4)]
      wvertex '4' [('1',2);('3',-5)]
      wvertex '5' [('4',6)]
    ]
  let h = 
    dict [
      '1', 0
      '2', -1
      '3', -5
      '4', 0
      '5', -4
    ]
  [<Test>]
  member this.BellmanFordProvidesReweightBaseline () =
    Assert.That (AllPairsShortestPath.nonnegativeBaseline (makeGraph ()), Is.EqualTo h)
  [<Test>]
  member this.JohnsonReweightProducesNonNegativeEdges () =
    let g' = 
      Dictionary.fromList [
        wvertex '1' [('2',4);('3',13);('5',0)]
        wvertex '2' [('4',0);('5',10)]
        wvertex '3' [('2',0)]
        wvertex '4' [('1',2);('3',0)]
        wvertex '5' [('4',2)]
      ]
    let g = makeGraph ()
    AllPairsShortestPath.reweight g h
    AssertGraphWeightEquals g g'
  [<Test>]
  member this.JohnsonReturnsAllPairsShortestPathMatrix () =
    let d = Dictionary<char*char,int>()
    d.[('1','1')] <- 0
    d.[('1','2')] <- 1
    d.[('1','3')] <- -3
    d.[('1','4')] <- 2
    d.[('1','5')] <- -4

    d.[('2','1')] <- 3
    d.[('2','2')] <- 0
    d.[('2','3')] <- -4
    d.[('2','4')] <- 1
    d.[('2','5')] <- -1

    d.[('3','1')] <- 7
    d.[('3','2')] <- 4
    d.[('3','3')] <- 0
    d.[('3','4')] <- 5
    d.[('3','5')] <- 3

    d.[('4','1')] <- 2
    d.[('4','2')] <- -1
    d.[('4','3')] <- -5
    d.[('4','4')] <- 0
    d.[('4','5')] <- -2

    d.[('5','1')] <- 8
    d.[('5','2')] <- 5
    d.[('5','3')] <- 1
    d.[('5','4')] <- 6
    d.[('5','5')] <- 0

    Assert.That (AllPairsShortestPath.johnson (makeGraph()), Is.EqualTo d)