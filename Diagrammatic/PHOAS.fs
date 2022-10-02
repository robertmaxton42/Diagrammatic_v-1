namespace Diagrammatic
open Higher
open Lazy
<<<<<<< Updated upstream
open NatTypes
open FunctorExt
=======
//open NatTypes
open FunctorExt
open Fix
>>>>>>> Stashed changes

module PHOGraph =

  //let rec lazyfix (f:Lazy<'a> -> Lazy<'a>) =  f <<= (lazyfix f)

  //let lazy_itemwise lzyL = 
  //  Seq.init l

<<<<<<< Updated upstream
  let force (x: Lazy<_>) = x.Force ()
  let delay x = lazy x

  let fix f =
    let rec fix' f = lazy f (fix' f)
    force (fix' f)

  let itfix f = 
    let rec itfix' f = Seq.map (f << itfix') << f
    itfix' f
=======
  //let itfix f = 
  //  let rec itfix' f = Seq.map (f << itfix') << f
  //  itfix' f
>>>>>>> Stashed changes

  //let rec fix f x = f (fix f) x
  //let eval thunk = thunk ()
  //let thunkify v = fun () -> v
  //let lift_thunk f = fun thunk -> fun () -> f (thunk ())
  //let lower_thunk (f: (unit -> 'a) -> (unit -> 'b)) = eval << f << thunkify

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
    | Mu of (Lazy<'a> seq -> App<'F, GraphRec<'F, 'a>> seq) 
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
      let v' = f v
      if v = v' then v else fixVal v' f

<<<<<<< Updated upstream
    let gfold (maplink: 't -> 'c) (fixreduce: (Lazy<'t> seq -> Lazy<'c> seq) -> 'c) (ev: #Functor<'F>) (freduce: App<'F, Lazy<'c>> -> 'c) = 
=======
    let gfold (maplink: 't -> 'c) (fixreduce: (Lazy<'t seq> -> 'c seq) -> 'c) (ev: #Functor<'F>) (freduce: App<'F, Lazy<'c>> -> 'c) = 
>>>>>>> Stashed changes
      let rec transform =
        //let mapreduce = freduce << ev.LazyMap transform << force
        //let cyclef (cycle: 't seq -> App<'F, GraphRec<'F, 't>> seq) = (function | Lazy(x) -> lazy (cycle x))
        function
        | Ref x -> maplink x
        | Mu cycle ->
            let transreduce x = lazy (freduce << ev.LazyMap transform) x
<<<<<<< Updated upstream
            fixreduce (fun branchesin -> let branchesout = cycle branchesin in Seq.map transreduce branchesout)
=======
            fixreduce ((Seq.map transreduce) << cycle)
            //fixreduce (fun branchesin -> let branchesout = cycle branchesin in Seq.map transreduce branchesout)
>>>>>>> Stashed changes
            //fixreduce (fun branches -> 
            //              lazy (Seq.map (freduce << ev.LazyMap transform) << cycle) ((Seq.map (freduce << ev.LazyMap transform) << cycle) (force branches)))
        | In subg -> (freduce << ev.LazyMap transform) subg
      transform


    let fold (ev: #Functor<'F>) (freduce: App<'F, Lazy<'c>> -> 'c) (k: 'c) =
      gfold id (fun g -> (force << Seq.head << g) (Seq.initInfinite (fun _ -> lazy k))) ev freduce
<<<<<<< Updated upstream

    let cfold (ev: #Functor<'F>) (freduce: App<'F, Lazy<'t>> -> 't) =
      gfold id (Seq.head << fix) ev freduce

=======

    //let cfold (ev: #Functor<'F>) (freduce: App<'F, Lazy<'t>> -> 't) =
    //  gfold id (Seq.head << fix) ev freduce

    let cfold (ev: #Functor<'F>) (freduce: App<'F, Lazy<'t>> -> 't) =
      let rec transform =
        function
        | Ref x -> x
        | Mu cycle ->
            let transreduce x = lazy (freduce << ev.LazyMap transform) x
            Seq.head << fix ((Seq.map transreduce) << cycle)
        | In subg -> (freduce << ev.LazyMap transform) subg
      transform

>>>>>>> Stashed changes
    let sfold (ev: #Functor<'F>) (freduce: App<'F, Lazy<'t>> -> 't) (k: 't) =
      gfold id (Seq.head << ((fixVal (Seq.initInfinite (fun _ -> k))) << lower_thunk)) ev freduce

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

    //let elems<'a> =
    //  PHOGraph.Graph.fold (new StreamBaseFunctor<'a>()) streamf2list []
    
    //let linearize<'a> =
    //  PHOGraph.Graph.cfold (new StreamBaseFunctor<'a>()) streamf2seq
  
<<<<<<< Updated upstream
  let onetwof = function 
    | branchesin -> 
        seq {
            Cons (1, PHOGraph.In ( StreamBase.Inj <| 
                    Cons (2, (PHOGraph.Ref (Seq.head branchesin)))
            )) |> StreamBase.Inj
        } 

  let onetwo = PHOGraph.Mu onetwof
=======
    let linearize<'a> (ev: #Functor<'F>) =
      let rec transform =
        function
        | PHOGraph.GraphRec.Ref x -> x
        | PHOGraph.GraphRec.Mu cycle ->
            let transreduce x = lazy (streamf2seq << ev.LazyMap transform) x
            Seq.head << fix ((Seq.map transreduce) << cycle)
        | PHOGraph.GraphRec.In subg -> (streamf2seq << ev.LazyMap transform) subg
      transform

>>>>>>> Stashed changes


