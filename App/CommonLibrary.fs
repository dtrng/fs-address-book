module CommonLibrary

type ValidationError = ValidationError of string

[<RequireQualifiedAccess>]
module Result = 
  let apply fResult xResult =
    match fResult, xResult with
    | Ok f, Ok x -> Ok (f x)
    | Error e, Ok _ -> Error e
    | Ok _, Error e -> Error e
    | Error ef, Error ex -> Error (ef @ ex)
