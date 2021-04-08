namespace Higher

[<AbstractClass>]
type Brand<'F> private () =
  abstract member token: unit -> Brand<'F>
  abstract member Inj
