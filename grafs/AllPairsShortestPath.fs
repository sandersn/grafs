module AllPairsShortestPath
open System.Collections.Generic
open System
open Types
open Util
open Util.TupleArrow
// External is a copy of Option needed because None translates to null in C#,
// and null is not allowed as a key in Dictionary
[<DefaultAugmentation(false)>]
[<StructuralEquality>]
[<StructuralComparison>]
type External<'T> =
| External
| Internal of 'T
with
  member this.Value =
    match this with
    | External -> raise (NullReferenceException "External has no Value")
    | Internal x -> x

let addExternal (g : Graph<'k,unit,int>) =
  let g' = Dictionary (Seq.length g + 1)
  for k in g.Keys do
    let originalEdges = g.[k].edges |> Seq.map (fun e -> {meta=e.meta; edge=Internal e.edge})
    g'.[Internal k] <- {meta=(); edges=ResizeArray originalEdges}
  g'.Add (wvertex External [for k in g'.Keys -> (k,0)])
  g'
let nonnegativeBaseline (g : Graph<'k,unit,int>) : IDictionary<'k,int> =
  match SingleSourceShortestPath.bellmanFord (addExternal g) External with
  | None -> raise <| ArgumentException ("Input contains a negative-weight cycle", "g")
  | Some g' -> dict [for {edge=e} in g'.[External].edges -> (e.Value, g'.[e].meta.distance)]
let reweight (g : Graph<'k,unit,int>) (h : IDictionary<'k,int>) = 
  for u in g.Keys do
    for v in g.[u].edges do
      v.meta <- v.meta + h.[u] - h.[v.edge]
// WARNING: destructively reweights g's edges to be non-negative
let johnson (g : Graph<'k,unit,int>) : IDictionary<'k*'k,int> =
  let d = Dictionary ()
  let h = nonnegativeBaseline g
  reweight g h
  for u in g.Keys do
    let g' = SingleSourceShortestPath.dijkstra g u
    for v in g.Keys do
      d.[(u,v)] <- g'.[v].meta.distance + h.[v] - h.[u]
  d :> IDictionary<'k*'k,int>