module SingleSourceShortestPathTest
open NUnit.Framework
open System.Collections.Generic
open Types
open Basic
open Util

[<TestFixture>]
type public SSSPathTester () =
  [<Test>]
  member this.BellManFordSetsCorrectParentsAndDistances () =
    let g = 
      Dictionary.fromList [
        wvertex 's' [('t',6);('y',7)]
        wvertex 't' [('x',5);('y',8);('z',-4)]
        wvertex 'x' [('t',-2)]
        wvertex 'y' [('x',-3);('z',9)]
        wvertex 'z' [('s',2);('x',7)]
      ]
    let res = SingleSourceShortestPath.bellmanFord g 's'
    Assert.That res.IsSome
    let g' = res.Value
    // specification of None as option<char> is necessary to appease
    // the interaction of F#'s strict-ish type system with NUnit's loose-ish typing.
    Assert.That (g'.['s'].meta, Is.EqualTo {parent=(None : option<char>); distance=0})
    Assert.That (g'.['t'].meta, Is.EqualTo {parent=Some 'x'; distance=2})
    Assert.That (g'.['x'].meta, Is.EqualTo {parent=Some 'y'; distance=4})
    Assert.That (g'.['y'].meta, Is.EqualTo {parent=Some 's'; distance=7})
    Assert.That (g'.['z'].meta, Is.EqualTo {parent=Some 't'; distance= -2})
  [<Test>]
  member this.DagShortestPathsSetsCorrectParentsAndDistances () =
    let g = 
      Dictionary.fromList [
        wvertex 'r' [('s',5);('t',3)]
        wvertex 's' [('t',2);('x',6)]
        wvertex 't' [('x',7);('y',4);('z',2)]
        wvertex 'x' [('y',-1);('z',1)]
        wvertex 'y' [('z',-2)]
        wvertex 'z' []
      ]
    let g' = SingleSourceShortestPath.dagShortestPaths g 's'
    Assert.That (g'.['r'].meta, Is.EqualTo {parent=(None : option<char>);
                                            distance=System.Int32.MaxValue})
    Assert.That (g'.['s'].meta, Is.EqualTo {parent=(None : option<char>); distance=0})
    Assert.That (g'.['t'].meta, Is.EqualTo {parent=Some 's'; distance=2})
    Assert.That (g'.['x'].meta, Is.EqualTo {parent=Some 's'; distance=6})
    Assert.That (g'.['y'].meta, Is.EqualTo {parent=Some 'x'; distance=5})
    Assert.That (g'.['z'].meta, Is.EqualTo {parent=Some 'y'; distance=3})
  [<Test>]
  member this.DijkstraSetsCorrectParentsAndDistances () =
    let g = 
      Dictionary.fromList [
        wvertex 's' [('t',10);('y',5)]
        wvertex 't' [('x',1);('y',2)]
        wvertex 'x' [('z',4)]
        wvertex 'y' [('t',3);('x',9);('z',2)]
        wvertex 'z' [('s',7);('x',6)]
      ]
    let g' = SingleSourceShortestPath.dijkstra g 's'
    // specification of None as option<char> is necessary to appease
    // the interaction of F#'s strict-ish type system with NUnit's loose-ish typing.
    Assert.That (g'.['s'].meta, Is.EqualTo {parent=(None : option<char>); distance=0})
    Assert.That (g'.['t'].meta, Is.EqualTo {parent=Some 'y'; distance=8})
    Assert.That (g'.['x'].meta, Is.EqualTo {parent=Some 't'; distance=9})
    Assert.That (g'.['y'].meta, Is.EqualTo {parent=Some 's'; distance=5})
    Assert.That (g'.['z'].meta, Is.EqualTo {parent=Some 'y'; distance=7})
  [<Test>]
  member this.DijkstraSetsCorrectParentsAndDistancesForJohnsonReweightedGraph () =
    let g = 
      Dictionary.fromList [
        wvertex '1' [('2',4);('3',13);('5',0)]
        wvertex '2' [('4',0);('5',10)]
        wvertex '3' [('2',0)]
        wvertex '4' [('1',2);('3',0)]
        wvertex '5' [('4',2)]
      ]
    let g' = SingleSourceShortestPath.dijkstra g '1'
    Assert.That (g'.['1'].meta, Is.EqualTo {parent=(None : option<char>); distance=0})
    Assert.That (g'.['2'].meta, Is.EqualTo {parent=Some '3'; distance=2})
    Assert.That (g'.['3'].meta, Is.EqualTo {parent=Some '4'; distance=2})
    Assert.That (g'.['4'].meta, Is.EqualTo {parent=Some '5'; distance=2})
    Assert.That (g'.['5'].meta, Is.EqualTo {parent=Some '1'; distance=0})