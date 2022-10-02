namespace Diagrammatic

open Lazy
module Fix =
    let force (x: Lazy<_>) = x.Force ()
    let delay x = lazy x

    let fix f =
        let rec fix' f = lazy f (fix' f)
        force (fix' f)
