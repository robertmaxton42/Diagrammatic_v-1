module Lazy 
  let inline force (x: Lazy<_>) = x.Force()
  //let (=<<) l1 l2 = lazy (force ((force l1) |<< l2))
  //let (=<<) f1 f2 = f1 << (fun x -> lazy f2 x)

  type LazyBuilder () =
    //let force (x : Lazy<'T>) = x |> function | Lazy x -> x
    /// bind
    let (>>=) (x:Lazy<'a>) (f:('a -> Lazy<'a>)) : Lazy<'a> =
      lazy (x |> force |> f |> force)  
    /// zero
    let lazyzero = Seq.empty
    
    member this.Bind(x, f) = x >>= f
    member this.Return (x) = lazy x
    member this.ReturnFrom (x) = x
    member this.Zero() = lazyzero 
    member this.Delay f = f()

    member this.Combine (a,b) = 
      Seq.append a b 

    member this.Combine (a,b) = 
      Seq.append (Seq.singleton a) b

    member this.Combine (a,b) = 
      Seq.append a (Seq.singleton b) 

    member this.Combine (Lazy(a), Lazy(b)) = 
      lazy Seq.append a b 

    member this.Combine (a:Lazy<'T>, b:Lazy<seq<'T>>) = 
      (a,b) |> function | (Lazy x, Lazy y) -> lazy Seq.append (Seq.singleton x) y

    member this.Combine (a:Lazy<seq<'T>>, Lazy(b)) = 
      a |> function | Lazy x -> lazy Seq.append x (Seq.singleton b) 

    member this.Combine (a:Lazy<seq<'T>>, b:seq<Lazy<'T>>) =
      let notlazy (xs:seq<Lazy<'T>>) = xs |> Seq.map (fun (Lazy(a)) -> a)
      a |> function | Lazy x -> lazy Seq.append x (notlazy b) 

    member this.Combine (a:seq<Lazy<'T>>, b:Lazy<seq<'T>>) =
      let notlazy (xs:seq<Lazy<'T>>) = xs |> Seq.map (fun (Lazy(a)) -> a)
      b |> function | Lazy x -> lazy Seq.append (notlazy a) x  

    member this.For (s,f) =
      s |> Seq.map f 

    member this.Yield x = lazy x
    member this.YieldFrom x = x

    member this.Run (x:Lazy<'T>) = x
    member this.Run (xs:seq<Lazy<unit>>) = 
      xs |> Seq.reduce (fun a b -> this.Bind(a, fun _ -> b)) 
    member this.Run (xs:seq<Lazy<'T>>) = 
      xs |> Seq.map (fun (Lazy(a)) -> a) |> fun x -> lazy x

  let lazyz = LazyBuilder () 
  let (|<<) f (Lazy(x)) = lazy f x  
  let (<<|) f x = f (lazy x)
  let (<|<) lazyf f = fun x -> lazyf (lazy f x)
  let (<<=) (f: 'a -> Lazy<'b>) x = lazy (force << f <| force x)