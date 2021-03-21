module UseCases
open CommonLibrary
open AddressBook

///
/// ADD Usecase
///
module ADD =
  type UnvalidatedContact = {
    FirstName: Option<string>
    LastName: Option<string>
    EmailAddress: Option<string>
    PhoneNumber: Option<string>
  }

  type ValidateContact = UnvalidatedContact -> Result<Contact, ValidationError list>

  let validateContact : ValidateContact =
    fun unvalidatedContact ->
      let emailResult = 
        unvalidatedContact.EmailAddress 
        |> Option.map (EmailAddress.create >> Result.mapError (ValidationError >> List.singleton))
        |> Option.orElse (ValidationError "Email Address is required" |> List.singleton |> Error |> Some)
        |> Option.get

      let firstNameResult = 
        unvalidatedContact.FirstName 
        |> Option.map (FirstName.create >> Result.mapError (ValidationError >> List.singleton))
        |> Option.orElse (ValidationError "First Name is required" |> List.singleton |> Error |> Some)
        |> Option.get

      let lastNameResult = 
        unvalidatedContact.LastName 
        |> Option.map (LastName.create >> Result.mapError (ValidationError >> List.singleton))
        |> Option.orElse (ValidationError "Last Name is required" |> List.singleton |> Error |> Some)
        |> Option.get

      let phoneResult = 
        unvalidatedContact.PhoneNumber 
        |> Option.map (PhoneNumber.create >> Result.mapError (ValidationError >> List.singleton))
        |> Option.orElse (ValidationError "Phone Number is required" |> List.singleton |> Error |> Some)
        |> Option.get

      let (<!>) = Result.map
      let (<*>) = Result.apply

      createContact <!> firstNameResult <*> lastNameResult <*> emailResult <*> phoneResult

  let exec (input: UnvalidatedContact) =
    let result = 
      input
      |> validateContact

    match result with
    | Ok contact -> sprintf "%s %s added" (FirstName.value contact.FirstName) (LastName.value contact.LastName) |> Ok
    | Error errors -> sprintf "Failed to add contact due to: %A" errors |> Error



///
/// DELETE Usecase
///
module DELETE =
  type Input = {
    Id: Option<string>
  }

  let exec (input: Input) =
    Error "does not exist"


///
/// LIST Usecase
///
module LIST =
  let exec () =
    Error "No items"