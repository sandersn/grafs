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
  cConnected : int
}
module Dictionary =
  let fromList pairs = 
    let d = Dictionary (Seq.length pairs)
    for k,v in pairs do
      d.[k] <- v
    d
let annotate (g: Graph<'k,'a>) (meta : 'b) : Graph<'k,'b> =
  let g' = Dictionary (Seq.length g)
  for pair in g do
    g'.[pair.Key] <- {meta=meta; edges=pair.Value.edges }
  g'
let reverse (g : Graph<'a,'b>) : Graph<'a,unit> =
  let g' = Dictionary (Seq.length g)
  for pair in g do
    g'.[pair.Key] <- {meta=(); edges=ResizeArray () }
  for pair in g do
    for k' in pair.Value.edges do
      g'.[k'].edges.Add pair.Key
  g'
let bfs (g : Graph<'k,unit>) (start : 'k) : Graph<'k,Bfs<'k>> =
  let g' = annotate g {colour=White; depth=Int32.MaxValue; parent=None}
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

let dfsWith (g : Graph<'k,unit>) action : Graph<'k,Dfs<'k>> =
  let g' = annotate g {colour=White; discover=0; finish=0; parent=None; cConnected= -1}
  let time = ref 0
  let rec dfsVisit (u : 'k) k =
    let value = g'.[u]
    time := !time + 1
    value.meta <- {value.meta with colour=Grey; discover=time.Value; cConnected=k}
    for v in value.edges |> Seq.filter (fun v -> g'.[v].meta.colour = White) do
      g'.[v].meta <- { g'.[v].meta with parent=Some u }
      dfsVisit v k
    time := !time + 1
    value.meta <- {value.meta with colour=Black; finish=time.Value}
    action u
  let mutable cConnected = 0
  for pair in g' do 
    if pair.Value.meta.colour = White then 
      dfsVisit pair.Key cConnected
      cConnected <- cConnected + 1
  g'
let dfs (g : Graph<'k,unit>) : Graph<'k,Dfs<'k>> = dfsWith g ignore

(*let stronglyConnectedComponents (g : Graph<'k,Dfs<'k>>) : Graph<'k,Dfs<'k>> =
  let g' = reverse g
  g
*)
let state v =
  let r = ref v
  let get () = r.Value
  let put v = r.Value <- v
  let update f = r.Value <- f r.Value
  get,put,update
let cons x l = x :: l

let topologicalSort (g : Graph<'k,unit>) =
  let get,_,update = state []
  dfsWith g (cons >> update) |> ignore
  get ()

printfn "This program is not intended for running. I created the wrong type of project."
