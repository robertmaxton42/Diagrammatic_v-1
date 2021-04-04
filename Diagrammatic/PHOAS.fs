namespace Diagrammatic
open Higher

module PHOGraph =

  let fix f = 
    let rec fix' f = lazy f (fix' f) 
    (fix' f).Value

  type Abstract = class end
  type GraphRec<'F, 'a> = 
    private
    | Var of 'a
    | Mu of ('a seq -> App<'F, GraphRec<'F, 'a>> seq)
    | In of App<'F, GraphRec<'F, 'a>>

  type Graph<'F> = private Graph of GraphRec<'F, Abstract>
  
  module Graph =
    let hide gr = Graph gr
    let reveal (Graph g) = g

    let private force (x: Lazy<'a>) = x.Value

    let rec private fixVal (v: 'a when 'a : equality) f = 
      let v' = f (Lazy.CreateFromValue v)
      if v = v' then v else fixVal v' f


    let gfold (link: 't -> 'c) (fixreduce: (Lazy<'t seq> -> 'c seq) -> 'c) (ev: #Functor<'F>) (reduce: App<'F, 'c> -> 'c) = 
      let rec trans graph = 
        match graph with
        | Var x -> link x
        // The argument to fixmap should be able to take an infinite series of constants
        // and take only as many as it needs (only as many as there are actual branches
        // from the current node)
        | Mu cycle -> fixreduce (Seq.map (reduce << ev.Map trans) << cycle << force)
        | In subg -> reduce (ev.Map trans subg)
      trans << reveal 

    let fold (ev: #Functor<'F>) (alg: App<'F, 'c> -> 'c) (k: 'c) =
      gfold id (fun g -> (Seq.head << g) (lazy Seq.initInfinite (fun _ -> k))) ev alg

    let cfold (ev: #Functor<'F>) (reduce: App<'F, 't> -> 't)=
      gfold id (Seq.head << fix) ev reduce

    let sfold (ev: #Functor<'F>) (alg: App<'F, 't> -> 't) (k: 't) =
      gfold id (Seq.head << (fixVal (Seq.initInfinite (fun _ -> k)))) ev alg

  // type Graph <'a, 'v> =
  //   private
  //   | 


  // type PLambda<'a> =
  //   private
  //   | Var 'a
  //   | Int int
  //   | Bool bool
  //   | If (PLambda 'a) (PLambda 'a) (PLambda 'a)
  //   | Add (PLambda 'a) (PLambda 'a)
  //   | Mult (PLambda 'a) (PLambda 'a)
  //   | Eq (PLambda 'a) (PLambda 'a)
  //   | Lam ('a -> PLambda 'a)
  //   | Mu1 ('a -> PLambda 'a)