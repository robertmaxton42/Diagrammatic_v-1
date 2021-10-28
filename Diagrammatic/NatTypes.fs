namespace Diagrammatic

module NatTypes =

  type Nat = interface end
  type NatZero = 
    inherit Nat  
  type NatSucc<'a when 'a :> Nat> =
    inherit Nat
  type NatSomeSucc = 
    inherit Nat
    abstract Apply : NatSomeApp<'z> -> 'z 
  and NatSomeApp<'z> =
    abstract Apply : NatSucc<'x> -> 'z


  //type NList<'N, 'a when 'N :> NatZero> = | Empty 

  type NList<'N, 'a when 'N :> NatSucc<#Nat>> = Cons of 'a * NList<'M, 'a>
    