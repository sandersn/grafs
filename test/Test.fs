﻿module Test
open NUnit.Framework
open System.Collections.Generic
open Basic

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
    Assert.That ({meta=(); edges=ResizeArray [1;2;3]}, Is.EqualTo {meta=(); edges=ResizeArray [1;2;3]})
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
    let check node parent discover finish =
      Assert.That (sorted.[node].meta.discover, Is.EqualTo discover)
      Assert.That (sorted.[node].meta.finish, Is.EqualTo finish)
      Assert.That (sorted.[node].meta.colour, Is.EqualTo Black)
      Assert.That (sorted.[node].meta.parent, Is.EqualTo parent)
    check 'u' None 1 8
    check 'v' (Some 'u') 2 7
    check 'w' None 9 12
    check 'x' (Some 'y') 4 5
    check 'y' (Some 'v') 3 6
    check 'z' (Some 'w') 10 11