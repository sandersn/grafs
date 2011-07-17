module Test
open NUnit.Framework
open System.Collections.Generic
open Types
open Basic
open Util

[<TestFixture>]
type public Tester() =
  let g = 
    Dictionary.fromList [
      vertex 1 [2; 3; 4]
      vertex 2 [1]
      vertex 3 [3;4]
      vertex 4 []
      vertex 5 [4]
    ]
  let g' = 
    Dictionary.fromList [
      vertex 1 [2]
      vertex 2 [1]
      vertex 3 [1;3]
      vertex 4 [1;3;5]
      vertex 5 []
    ]
  [<Test>]
  member this.VertexSupportsEquality () =
    Assert.That ({meta=(); edges=ResizeArray [{edge=1;meta=()}
                                              {edge=2;meta=()}
                                              {edge=3;meta=()}]}, 
                 Is.EqualTo {meta=(); edges=ResizeArray [{edge=1;meta=()}
                                                         {edge=2;meta=()}
                                                         {edge=3;meta=()}]})
  [<Test>]
  member this.VertexEqualityIgnoresMetadata () =
    Assert.That ({meta=1; edges=ResizeArray [{edge=1;meta=()};{edge=2;meta=()}]},
                 Is.EqualTo {meta=2; edges=ResizeArray [{edge=1;meta=()};{edge=2;meta=()}]})
  [<Test>]
  member this.ReverseEmptyGraphIsEmptyGraph () =
    Assert.That (Basic.reverse (Dictionary ()),Is.EqualTo (Dictionary ()))
  [<Test>]
  member this.ReverseSimpleGraphReversesEdges () =
    Assert.That (Basic.reverse g,Is.EqualTo g')
  [<Test>]
  member this.BfsCreatesSimpleTree () =
    let g =
      Dictionary.fromList [
        vertex 'r' ['s'; 'v']
        vertex 's' ['r'; 'w']
        vertex 't' ['u'; 'w'; 'x']
        vertex 'u' ['t'; 'x'; 'y']
        vertex 'v' ['r']
        vertex 'w' ['s'; 't'; 'x']
        vertex 'x' ['t'; 'u'; 'w'; 'y']
        vertex 'y' ['u'; 'x']
      ]
    let sorted = Basic.bfs g 's'
    let check node parent depth = 
      Assert.That (sorted.[node].meta.depth, Is.EqualTo depth)
      Assert.That (sorted.[node].meta.colour, Is.EqualTo Black)
      Assert.That (sorted.[node].meta.parent, Is.EqualTo (Some parent))
    check 'r' 's' 1
    check 'w' 's' 1
    check 'v' 'r' 2
    check 't' 'w' 2
    check 'x' 'w' 2
    check 'u' 't' 3
    check 'y' 'x' 3
  [<Test>]
  member this.DfsAnnotatesGraph () =
    let g =
      Dictionary.fromList [
        vertex 'u' ['v'; 'x']
        vertex 'v' ['y']
        vertex 'w' ['y'; 'z']
        vertex 'x' ['v']
        vertex 'y' ['x']
        vertex 'z' ['z']
      ]
    let sorted = Basic.dfs g
    let check node parent discover finish k =
      Assert.That (sorted.[node].meta.discover, Is.EqualTo discover)
      Assert.That (sorted.[node].meta.finish, Is.EqualTo finish)
      Assert.That (sorted.[node].meta.colour, Is.EqualTo Black)
      Assert.That (sorted.[node].meta.parent, Is.EqualTo parent)
      Assert.That (sorted.[node].meta.cConnected, Is.EqualTo k)
    check 'u' None 1 8 0
    check 'v' (Some 'u') 2 7 0
    check 'w' None 9 12 1
    check 'x' (Some 'y') 4 5 0
    check 'y' (Some 'v') 3 6 0
    check 'z' (Some 'w') 10 11 1
  [<Test>]
  member this.TopologicalSortIsCorrect () =
    let g =
      Dictionary.fromList [
        vertex "shirt" ["tie"; "belt"]
        vertex "tie" ["jacket"]
        vertex "jacket" []
        vertex "belt" ["jacket"]
        vertex "watch" []
        vertex "boxers" ["pants"; "shoes"]
        vertex "pants" ["belt"; "shoes"]
        vertex "shoes" []
        vertex "socks" ["shoes"]
      ]
    Assert.That (Basic.topologicalSort g,
                 Is.EqualTo ["socks"; "boxers"; "pants"; "shoes"; "watch"; "shirt"; "belt"; "tie"; "jacket"])
  [<Test>]
  member this.StronglyConnectedComponentsAreCorrect () =
    let g =
      Dictionary.fromList [
        vertex 'c' ['g'; 'd']
        vertex 'g' ['f'; 'h']
        vertex 'f' ['g']
        vertex 'h' ['h']
        vertex 'd' ['c']
        vertex 'b' ['c'; 'f'; 'e']
        vertex 'e' ['f'; 'a']
        vertex 'a' ['b']
      ]
    Assert.That (Basic.stronglyConnectedComponents g,
                 Is.EqualTo (set [set ['a'; 'b'; 'e']; set ['c'; 'd']; set ['f'; 'g']; set ['h']]))