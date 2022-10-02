#r "..\paket-files\github.com\palladin\Higher\src\Higher\obj\Release\Higher.dll";;
#load "Lazy.fs";;
#load "LazyFunctor.fs"
#load "PHOAS.fs";;

open Diagrammatic
open Higher
open PHOGraph
open PHOTypes

type ThunkElem<'a, 'r> = unit -> 'a * 'r 

//ThunkElem brand boilerplate
type ThunkElem private () =
  static let token = new ThunkElem()
  static member Inj (value : ThunkElem<'A, 'B>) : App2<ThunkElem, 'A, 'B> =
    App2<ThunkElem, 'A, 'B>.Create(AppToken<ThunkElem, 'A>.Token(token), value)
  static member Prj (app : App2<ThunkElem, 'A, 'B>) : ThunkElem<'A, 'B> =
    app.Apply(AppToken<ThunkElem, 'A>.Token(token)) :?> _

 type ThunkElemFunctor<'a> () =
   inherit Functor<App<ThunkElem, 'a>>()
   override self.Map<'A, 'B> (f: 'A -> 'B) thunk =
     (fun () ->
        match ((ThunkElem.Prj thunk) ()) with
        | (hd, tl) -> (hd, f tl)) |> ThunkElem.Inj


let append a l = Fix.Fix(ThunkElem.Inj (fun () -> (a, l)))
let lift_append a thunk = 
  thunk 
  |> ThunkElem.Inj
  |> Fix.Fix
  |> (append a)
  |> function (Fix (f)) -> ThunkElem.Prj f

let rec thunk2seq thunk =
  match (thunk ()) with
  | (hd, tl) -> seq { yield hd; yield! (thunk2seq << ThunkElem.Prj << Fix.un) tl }

let ones = fix (lift_append 1)


type ThunkList<'a> = Fix<App<ThunkElem, 'a>>