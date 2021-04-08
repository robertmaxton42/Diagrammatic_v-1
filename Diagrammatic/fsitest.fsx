#r "..\paket-files\github.com\palladin\Higher\src\Higher\obj\Release\Higher.dll";;
#load "PHOAS.fs";;

open Diagrammatic;;
open PHOGraph;;
open PHOTypes;;

let onetwo = Mu (fun branches -> seq {StreamBase.Inj <| Cons (1, In (StreamBase.Inj <| Cons (2, (Var (Seq.head branches)))))} )