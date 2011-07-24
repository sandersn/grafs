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

