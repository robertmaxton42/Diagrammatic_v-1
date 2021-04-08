namespace Diagrammatic
open Higher

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
    | Var of 'a
    | Mu of (Lazy<'a seq> -> App<'F, GraphRec<'F, 'a>> seq)
    | In of App<'F, GraphRec<'F, 'a>>

  type Graph<'F> = 
    //private 
      GraphR of GraphRec<'F, Abstract>
  
  module Graph =
    let hide gr = GraphR gr
    let reveal (GraphR g) = g

    //let private force (x: Lazy<'a>) = x.Value

    let rec private fixVal (v: 'a when 'a : equality) f = 
      let v' = f (Lazy.CreateFromValue v)
      if v = v' then v else fixVal v' f


    let gfold (link: 't -> 'c) (fixreduce: (Lazy<'t seq> -> 'c seq) -> 'c) (ev: #Functor<'F>) (freduce: App<'F, 'c> -> 'c) = 
      let rec trans graph = 
        match graph with
        | Var x -> link x
        // The argument to fixreduce should be able to take an infinite series of constants
        // and take only as many as it needs (only as many as there are actual branches
        // from the current node)
        | Mu cycle -> fixreduce (Seq.map (freduce << ev.Map trans) << cycle)
        | In subg -> freduce (ev.Map trans subg)
      trans

    let fold (ev: #Functor<'F>) (freduce: App<'F, 'c> -> 'c) (k: 'c) =
      gfold id (fun g -> (Seq.head << g) (lazy Seq.initInfinite (fun _ -> k))) ev freduce

    let cfold (ev: #Functor<'F>) (freduce: App<'F, 't> -> 't)=
      gfold id (Seq.head << fix) ev freduce

    let sfold (ev: #Functor<'F>) (freduce: App<'F, 't> -> 't) (k: 't) =
      gfold id (Seq.head << (fixVal (Seq.initInfinite (fun _ -> k)))) ev freduce

    //let pjoin (ev: #Functor<'F>) 

module PHOTypes =
  type StreamBase<'a, 'r> = Cons of hd: 'a * tl: 'r    

  //StreamBase brand boilerplate
  type StreamBase private () =
    static let token = new StreamBase()
    static member Inj (value : StreamBase<'A, 'R>) : App2<StreamBase, 'A, 'R> =
      App2<StreamBase, 'A, 'R>.Create(AppToken<StreamBase, 'A>.Token(token), value)
    static member Prj (app : App2<StreamBase, 'A, 'R>) : StreamBase<'A, 'R> =
      app.Apply(AppToken<StreamBase, 'A>.Token(token)) :?> _

  type StreamBaseFunctor<'a> () =
    inherit Functor<App<StreamBase,'a>>()
    override self.Map<'A, 'B> (f: 'A -> 'B) stream=
      let (Cons (hd, tl)) = StreamBase.Prj stream
      Cons (hd, f tl) |> StreamBase.Inj

  type Stream<'a> = PHOGraph.Graph<App<StreamBase, 'a>>

  module Stream =
    let streamf2list stream = 
      let (Cons (hd, tl)) = StreamBase.Prj stream
      hd :: tl

    let streamf2seq stream =
      let (Cons (hd, tl)) = StreamBase.Prj stream
      seq { yield hd; yield! tl }

    let elems<'a> =
      PHOGraph.Graph.fold (new StreamBaseFunctor<'a>()) streamf2list []
    
    let linearize<'a> =
      PHOGraph.Graph.cfold (new StreamBaseFunctor<'a>()) streamf2seq
  
  let onetwo = PHOGraph.Mu (fun branches -> seq {StreamBase.Inj <| Cons (1, PHOGraph.In (StreamBase.Inj <| Cons (2, (PHOGraph.Var (Seq.head << branches)))))} )

