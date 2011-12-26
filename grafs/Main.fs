module Main
open Types
open Util
printfn "This is a placeholder"
let g = 
  Dictionary.fromList [
    wvertex '1' [('2',4);('3',13);('5',0)]
    wvertex '2' [('4',0);('5',10)]
    wvertex '3' [('2',0)]
    wvertex '4' [('1',2);('3',0)]
    wvertex '5' [('4',2)]
  ]
let g' = SingleSourceShortestPath.dijkstra g '1'
