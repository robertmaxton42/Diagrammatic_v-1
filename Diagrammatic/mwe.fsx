namespace Diagrammatic

module PHOGraph =
  //let rec fix f x = f (fix f) x

  let fix f = 
    let rec fix' f = lazy f (fix' f)
    (fix' f).Value
  
  let force (lzv: Lazy<_>) = lzv.Value
  
  type ListE<'a> =
    | EmptyListR
    | ListRec of 'a * GraphRec<'a>
  and GraphRec<'a> =
    | Ref of 'a 
    | Mu of (Lazy<'a seq> -> ListE<'a> seq)

  type Abstract = class end
  type Graph =
    | GraphR of GraphRec<Abstract>

  module Graph =
    let gfold fixreduce listreduce =
      let rec transform =
        function
        | Ref x -> x
        | Mu cycle -> fixreduce (fun thunk -> (Seq.map (listreduce << Seq.map transform) << cycle) thunk)
      transform

    let cfold listreduce =
      gfold (Seq.head << fix) listreduce


  let ones = 
    Mu(
      function 
      | Lazy(branches) -> seq { seq { Ref (Seq.head branches) } }
    )
  
  let linear = Graph.cfold 