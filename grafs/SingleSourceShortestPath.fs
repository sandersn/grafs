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
  if g.[u].meta.distance <> System.Int32.MaxValue && g.[v].meta.distance > distance' 
  then g.[v].meta <- {g.[v].meta with distance=distance'; parent=Some u}
  else ()
let bellmanFord (g : Graph<'k,unit,int>) source =
  let g' = init g source
  for i = 0 to g.Count - 2 do
    Seq.iter (relax g') (edges g')
  if edges g' |> Seq.exists (fun (w,(u,v)) -> g'.[v].meta.distance > g'.[u].meta.distance + w)
  then None
  else Some g'
let dagShortestPaths (g : Graph<'k,unit,int>) source =
  let g' = init g source
  for u in Basic.topologicalSort g do
    for v in g'.[u].edges do
      relax g' (v.meta,(u,v.edge))
  g'
let dijkstra (g : Graph<'k,unit,int>) source =
  let g' = init g source
  let mutable s = set []
  let q = MinQueue.MinQueue (g'.Keys, (fun v -> g'.[v].meta.distance))
  while q.Count <> 0 do
    let u = q.ExtractMin ()
    s <- Set.add u s // what is this good for? debugging? theoretical happiness?
    for v in g'.[u].edges do
      relax g' (v.meta,(u,v.edge))
  g'