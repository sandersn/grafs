module Span
open Types
open Util

let min (g : Graph<'k,unit,int>) =
  let mutable a = set []
  let forest = g.Keys |> Seq.map (fun v -> v, set [v]) |> Dictionary.fromList
  
  let edgePairs (k,{edges=edges}) =
    let undirected {edge=e; meta=w} =
      if e > k
      then (w,(k,e))
      else (w,(e,k))
    edges |> Seq.map undirected
  let edges = g |> Seq.map (Dictionary.item >> edgePairs) |> Seq.concat |> set |> Seq.sortBy fst |> Seq.map snd
  // TODO: Find a set in ks containg the arg
  let findSet x = forest |> Seq.find (fun pair -> pair.Value.Contains x) |> Dictionary.Key
  let union (dTree,sTree) = 
    forest.[dTree] <- Set.union forest.[dTree] forest.[sTree]
    forest.Remove sTree |> ignore
  for (dst,src) in edges do
    let dTree,sTree = findSet dst, findSet src
    if dTree <> sTree then
      a <- Set.add (dst,src) a
      union (dTree,sTree)
  a