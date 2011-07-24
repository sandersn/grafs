module SingleSourceShortestPath
open System.Collections.Generic
open Types
open Util
open Util.TupleArrow

let init (g : Graph<'k,unit,int>) source =
  let g' = annotate g {distance=System.Int32.MaxValue; parent=None}
  g'.[source].meta <- { g'.[source].meta with distance=0 }
  g'
let relax (g : Graph<'k,SingleSource<'k>,int>) (w,(u,v)) =
  let distance' = g.[u].meta.distance + w
  if g.[v].meta.distance > distance' 
  then g.[v].meta <- {g.[v].meta with distance=distance'; parent=Some u}
  else ()
let bellmanFord (g : Graph<'k,unit,int>) source =
  let g' = init g source
  for i = 0 to g.Count - 2 do
    Seq.iter (relax g') (edges g')
  if edges g' |> Seq.exists (fun (w,(u,v)) -> g'.[v].meta.distance > g'.[u].meta.distance + w)
  then None
  else Some g'
  
