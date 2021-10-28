namespace Diagrammatic
open Higher

module FunctorExt =
  type Functor<'F> with
    member self.LazyMap<'A, 'B> (f: 'A -> 'B) = self.Map (fun x -> lazy f x)