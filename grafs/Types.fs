module Types
open System.Collections.Generic

[<CustomEquality;NoComparison>]
type Edge<'k,'a when 'k : equality> = {
  mutable meta : 'a
  edge : 'k
}
with
  override this.ToString () = sprintf "%A:%A" this.edge this.meta
  override this.Equals that =
    match that with
    | null -> false
    | :? Edge<'k,'a> as that -> this.edge = that.edge
    | _ -> false
  override this.GetHashCode () = 17 ^^^ hash this.edge
  static member Edge (e : Edge<_,_>) = e.edge
[<CustomEquality;NoComparison>]
type Vertex<'k,'a,'e when 'k : equality> = {
  mutable meta : 'a
  edges : ResizeArray<Edge<'k,'e>>
} with
  override this.ToString () = sprintf "%A\t%A" this.meta this.edges
  override this.Equals that =
    match that with
    | null -> false
    | :? Vertex<'k,'a,'e> as that ->
      this.edges.Count = that.edges.Count 
      && Seq.forall2 (=) this.edges that.edges
    | _ -> false
  override this.GetHashCode () = 17 ^^^ hash this.edges
// graph proper //
type Graph<'k,'v,'e when 'k : equality> = Dictionary<'k, Vertex<'k,'v,'e>>
let annotate (g: Graph<'k,'a,unit>) (meta : 'b) : Graph<'k,'b,unit> =
  let g' = Dictionary (Seq.length g)
  for pair in g do
    g'.[pair.Key] <- {meta=meta; edges=pair.Value.edges }
  g'
let vertex v (es : seq<'a>) =
  (v,{meta=(); edges=ResizeArray<Edge<'a,unit>> (es |> Seq.map (fun a -> {edge=a; meta=()}))})
let wvertex v (es : seq<'a * int>) =
  (v,{meta=(); edges=ResizeArray<Edge<'a,int>> (es |> Seq.map (fun (a,b) -> {edge=a; meta=b}))})
// metadata //
type Colour  = White | Grey | Black
type Bfs<'k> = {
  colour : Colour
  depth : int
  parent : option<'k>
}
type Dfs<'k> = {
  colour : Colour
  discover : int
  finish : int
  parent : option<'k>
  cConnected : int
}
type Prim<'k> = {
  key : int
  parent : option<'k>
}
