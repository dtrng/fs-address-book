module CommonLibrary

type ValidationError = ValidationError of string

[<RequireQualifiedAccess>]
module Result = 
  let isError x =
    match x with
    | Ok _ -> false
    | Error _ -> true

  let getError x =
    match x with
    | Ok _ -> None
    | Error e -> Some e

  let foldError f (results: Result<'a, 'b> seq) =
    let errors = 
      results 
      |> Seq.filter isError
      |> Seq.reduce f

    let oks = 
      results
      |> Seq.filter (isError >> not)
  
    Seq.append oks [errors]

  let apply fResult xResult =
    match fResult, xResult with
    | Ok f, Ok x -> Ok (f x)
    | Error e, Ok _ -> Error e
    | Ok _, Error e -> Error e
    | Error ef, Error ex -> Error (ef @ ex)
