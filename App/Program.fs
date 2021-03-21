open UseCases

let parseCommand args =
  let length = Array.length args
  match length with
   | 0 -> None
   | _ -> Some(args.[0])

[<EntryPoint>]
let main argv =

  // Step 1. Parse the command type. add/list/delete
  // Step 2. Extract input parameters and apply to the given usecase
  
  let command = parseCommand argv

  let result = 
    match command with
    | Some x -> 
      match x with
      | "add" -> ADD.exec {FirstName=Some "Donald"; LastName=Some "Duck"; EmailAddress=Some "donald@tower.quack"; PhoneNumber= Some "+12343343"}
      | "list" -> LIST.exec ()
      | "delete" -> DELETE.exec {Id=Some"blahaa"}
      | _ -> Error "Command not found"
    | None -> Error "No command entered"

  printfn "Result of execution: %A" result

  0 // return an integer exit code