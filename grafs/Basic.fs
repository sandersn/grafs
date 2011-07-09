module Basic
open System
open System.Collections.Generic

[<CustomEquality;NoComparison>]
type Vertex<'k,'a when 'k : equality> = {
  mutable meta : 'a
  edges : ResizeArray<'k>
} with
  override this.ToString () = sprintf "%A\t%A" this.meta this.edges
  override this.Equals that =
    match that with
    | null -> false
    | :? Vertex<'k,'a> as that ->
      this.edges.Count = that.edges.Count 
      && Seq.forall2 (fun x y -> x = y) this.edges that.edges
    | _ -> false
  override this.GetHashCode () = 17 ^^^ hash this.edges
type Graph<'k,'a when 'k : equality> = Dictionary<'k, Vertex<'k,'a>>
let vertex v (es : seq<'a>)  = (v,{meta=(); edges=ResizeArray<'a> es})

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
}
module Dictionary =
  let fromList pairs = 
    let d = Dictionary (Seq.length pairs)
    for k,v in pairs do
      d.[k] <- v
    d
let reverse (g : Graph<'a,'b>) : Graph<'a,unit> =
  let g' = Dictionary (Seq.length g)
  for k in g.Keys do
    g'.[k] <- {meta=(); edges=ResizeArray ()}
  for pair in g do
    for k' in pair.Value.edges do
      g'.[k'].edges.Add pair.Key
  g'
let bfs (g : Graph<'k,unit>) (start : 'k) : Graph<'k,Bfs<'k>> =
  let g' = Dictionary (Seq.length g)
  for pair in g do
    g'.[pair.Key] <- {edges=pair.Value.edges; meta={colour=White; depth=Int32.MaxValue; parent=None}}
  g'.[start].meta <- {colour=Grey; depth=0; parent=None}
  let rec loop : (list<'k> -> Graph<'k,Bfs<'k>>) = function
  | [] -> g'
  | u::queue ->
    let m = g'.[u].meta
    let vs = g'.[u].edges |> Seq.filter (fun v -> g'.[v].meta.colour = White) |> Seq.toList
    vs |> Seq.iter (fun v -> g'.[v].meta <- { colour=Grey; depth=m.depth + 1; parent=Some u })
    g'.[u].meta <- { m with colour = Black }
    loop (queue @ vs)
  loop [start]
let dfs (g : Graph<'k,unit>) : Graph<'k,Dfs<'k>> =
  let g' = Dictionary (Seq.length g)
  for pair in g do
    g'.[pair.Key] <- {edges=pair.Value.edges; meta={colour=White; discover=0; finish=0; parent=None}}
  let time = ref 0
  let rec dfsVisit (u : 'k) =
    let value = g'.[u]
    time := !time + 1
    value.meta <- {value.meta with colour=Grey; discover=time.Value}
    for v in value.edges |> Seq.filter (fun v -> g'.[v].meta.colour = White) do
      g'.[v].meta <- { g'.[v].meta with parent=Some u }
      dfsVisit v
    time := !time + 1
    value.meta <- {value.meta with colour=Black; finish=time.Value}
  seq { for pair in g' do 
          if pair.Value.meta.colour = White then 
            yield pair.Key } |> Seq.iter dfsVisit
  g'
let g = 
    Dictionary.fromList [
      vertex 1 [2; 3; 4]
      vertex 2 [1]
      vertex 3 [3;4]
      vertex 4 []
      vertex 5 [4]
    ]
Seq.iter (printfn "%A") (reverse g)
