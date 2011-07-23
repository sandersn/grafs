module Span
open System.Collections.Generic
open Types
open Util
open Util.TupleArrow
open MinQueue

type Forest<'k when 'k : comparison> (g : Graph<'k,unit,int>) =
  let forest = g.Keys |> Seq.map (fun v -> v, set [v]) |> Dictionary.fromList
  member this.findTree k = 
    forest |> Seq.find (fun pair -> pair.Value.Contains k) |> Dictionary.Key
  member this.union (dTree,sTree) = 
    forest.[dTree] <- Set.union forest.[dTree] forest.[sTree]
    forest.Remove sTree |> ignore

let sortedEdgePairs g = 
  let edgePairs (src,{edges=edges}) =
    let undirected {edge=dst; meta=w} =
      if dst > src
      then (w,(src,dst))
      else (w,(dst,src))
    edges |> Seq.map undirected
  g |> Seq.map (Dictionary.item >> edgePairs) 
    |> Seq.concat |> set 
    |> Seq.sortBy fst |> Seq.map snd
let minKruskal (g : Graph<'k,unit,int>) =
  let forest = Forest g
  set (seq { 
    for (dst,src) in sortedEdgePairs g do
      let (dTree,sTree) = both forest.findTree (dst,src)
      if dTree <> sTree then
        yield (dst,src)
        forest.union (dTree,sTree)
  })
let minPrim (g : Graph<'k,unit,int>) (root : 'k) =
  let g' = Dictionary (Seq.length g)
  for pair in g do
    g'.[pair.Key] <- {edges=pair.Value.edges; meta={key=System.Int32.MaxValue; parent=None}}
  g'.[root] <- {g'.[root] with meta={key=0; parent=None}}
  let q = MinQueue g'.Keys
  while q.Count > 0 do
    let u = q.ExtractMin ()
    for v in g'.[u].edges do // pretty sure you instead could do a filter, then a minBy
      if q.Contains v.edge && v.meta < g'.[v.edge].meta.key then
        g'.[v.edge] <- {g'.[v.edge] with meta={key=v.meta; parent=Some u}}
  set (seq {for v in g'.Keys do
              match g'.[v].meta.parent with
              | Some u -> yield (u,v)
              | None -> ()})