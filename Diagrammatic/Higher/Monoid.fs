﻿namespace Higher
open System

/// Monoid class
[<AbstractClass>]
type Monoid<'T>() =
    abstract Empty : 'T
    abstract Append : 'T -> 'T -> 'T

type Monoid private () =
  static let token = new Monoid()
  static member Inj (m : 'T) : App2<Monoid, 'A, 'B> =
    App2<Monoid, 'A, 'B>.Create(AppToken.Token(token), m)
  static member Prj (app2: App2<Monoid, 'A, 'B>) : 'T =
    app2.Apply(AppToken.Token(token)) :?> _
  static member Run (app2: App2<Monoid, 'A, 'B>) : 'T =
    Monoid.Prj app2

/// Monoids are single object categories
type MonoidCategory<'T>(m: Monoid<'T>) =
  inherit Category<Monoid>() with
    override self.Ident<'A> () : App2<Monoid, 'A, 'A> =
      Monoid.Inj m.Empty
    override self.Compose<'A, 'B, 'C> (ab:App2<Monoid, 'A, 'B>) (bc:App2<Monoid, 'B, 'C>) : App2<Monoid, 'A, 'C> =
      m.Append (Monoid.Prj ab) (Monoid.Prj bc) |> Monoid.Inj


// Basic Monoid instances
type StringMonoid() =
    inherit Monoid<string>() with
    override self.Empty = ""
    override self.Append x y = x + y

type ListMonoid<'T>() =
    inherit Monoid<'T list>() with
    override self.Empty = []
    override self.Append xs ys = List.append xs ys

type SeqMonoid<'T>() =
    inherit Monoid<seq<'T>>() with
    override self.Empty = Seq.empty
    override self.Append xs ys = Seq.append xs ys

type EndoMonoid<'T>() =
    inherit Monoid<'T -> 'T>() with
    override self.Empty = id
    override self.Append f g = f >> g
