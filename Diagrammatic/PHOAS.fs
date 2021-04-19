namespace Diagrammatic
open Higher
open Lazy

module PHOGraph =

  let fix f = 
    let rec fix' f = lazy f (fix' f) 
    (fix' f).Value

  //Current plan: Keep Abstract, make GraphRec public, but make the library only really accept Graphs and not arbitrary
  //GraphRec's. Or, something like that. The functions need to be able to operate on arbitrarily-typed GraphRecs, 
  //but nobody but the library should be able to instantiate them as anything but GraphRec<'F, Abstract>
  type Abstract = class end
  type GraphRec<'F, 'a> = 
    //private
    //Cross- or back-link to previously defined node
    | Ref of 'a 
    // Defines a map of tree roots to trees
    // First element is 'this node' by convention; folds other than gfold
    // are written with that assumption
    | Mu of ('a seq -> App<'F, GraphRec<'F, 'a>> seq) 
    //Recursive graph structure
    | In of App<'F, GraphRec<'F, 'a>>

  type Graph<'F> = 
    //private 
      GraphR of GraphRec<'F, Abstract>
  
  module Graph =
    let hide gr = GraphR gr
    let reveal (GraphR g) = g

    //let private force (x: Lazy<'a>) = x.Value

    let rec private fixVal v f = 
      let v' = f (lazy v)
      if v = v' then v else fixVal v' f

    let gfold (maplink: 't -> 'c) (fixreduce: (Lazy<'t seq> -> 'c seq) -> 'c) (ev: #Functor<'F>) (freduce: Lazy<App<'F, 'c>> -> 'c) = 
      let rec transform graph = 
        match graph with
        | Ref x -> maplink x
        //Theory: The unconditional force is okay because there's an implicit lazy wrapper in <|<
        | Mu cycle -> fixreduce (Seq.map (freduce <|< ev.Map transform) << cycle << force)
        //| Mu cycle -> fixreduce (fun (heads: Lazy<'t seq>) -> force (lazyz {
        //              let! hs = heads
        //              let branches = cycle hs
        //              let fmapreduce = freduce <|< ev.Map transform
        //              Seq.map fmapreduce branches
        //            }))  
        | In subg -> (freduce <|< ev.Map transform) subg
      transform

    let fold (ev: #Functor<'F>) (freduce: App<'F, 'c> -> 'c) (k: 'c) =
      gfold id (fun g -> (Seq.head << g) (lazy Seq.initInfinite (fun _ -> k))) ev (freduce << force)

    let cfold (ev: #Functor<'F>) (freduce: Lazy<App<'F, 't>> -> 't)=
      gfold id (Seq.head << fix) ev freduce

    let sfold (ev: #Functor<'F>) (freduce: Lazy<App<'F, 't>> -> 't) (k: 't) =
      gfold id (Seq.head << (fixVal (Seq.initInfinite (fun _ -> k)))) ev freduce

module PHOTypes =
  type StreamBase<'a, 'r> = Cons of hd: 'a * tl: 'r    

  //StreamBase brand boilerplate
  type StreamBase private () =
    static let token = new StreamBase()
    static member Inj (value : StreamBase<'A, 'R>) : App2<StreamBase, 'A, 'R> =
      App2<StreamBase, 'A, 'R>.Create(AppToken<StreamBase, 'A>.Token(token), value)
    static member Prj (app : App2<StreamBase, 'A, 'R>) : StreamBase<'A, 'R> =
      app.Apply(AppToken<StreamBase, 'A>.Token(token)) :?> _

  let (|StreamCons|) stream = 
    let (Cons (hd, tl)) = StreamBase.Prj stream
    (hd, tl)

  type StreamBaseFunctor<'a> () =
    inherit Functor<App<StreamBase,'a>>()
    override self.Map<'A, 'B> (f: 'A -> 'B) stream =
      match stream with
      | StreamCons (hd, tl) -> (Cons (hd, f tl)) |> StreamBase.Inj
    

  type Stream<'a> = PHOGraph.Graph<App<StreamBase, 'a>>

  module Stream =
    let streamf2list = function 
      | StreamCons (hd, tl) -> hd :: tl

    let streamf2seq = function
      | StreamCons (hd, tl) -> seq { yield hd; yield! tl }

    let elems<'a> =
      PHOGraph.Graph.fold (new StreamBaseFunctor<'a>()) streamf2list []
    
    let linearize<'a> =
      PHOGraph.Graph.cfold (new StreamBaseFunctor<'a>()) streamf2seq
  
  let lazyhead (s: Lazy<#seq<_>>) = Seq.head |<< s
  let lazyhead s = Seq.head |<< s
  let test s = Cons (2, PHOGraph.Ref (Seq.head |<< s))

  let onetwo = PHOGraph.Mu (fun branches -> 
    seq {
        Cons (1, PHOGraph.In ( StreamBase.Inj <| 
          Cons (2, (PHOGraph.Ref (Seq.head << branches)))
        )) |> StreamBase.Inj
    } )

