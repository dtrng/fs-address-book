module AddressBook 
open CommonLibrary
open System.Text.RegularExpressions

module EmailAddress =
  type T = EmailAddress of string

  let create (s: string) =
    if s = "" 
    then Error "Email must not be empty"
    else if Regex.IsMatch(s, @"^\S+@\S+\.\S+$") |> not
    then Error "Email has wrong format"
    else Ok (EmailAddress s)

  let value (EmailAddress e) = e

module FirstName =
  type T = FirstName of string

  let isValid input = 
    match input with
    | None -> Error "First Name is required"
    | Some x when x = "" -> Error "First Name must not be blank"
    | Some x -> Ok x

  let create (s: string) =
    match isValid (Some s) with
    | Ok _ -> Ok (FirstName s)
    | Error x -> Error x 

  let value (FirstName e) = e

module LastName =
  type T = LastName of string

  let isValid input = 
    match input with
    | None -> Error "Last Name is required"
    | Some x when x = "" -> Error "Last Name must not be blank"
    | Some x -> Ok x

  let create (s: string) =
    match isValid (Some s) with
    | Ok _ -> Ok (LastName s)
    | Error x -> Error x 

  let value (LastName e) = e

module PhoneNumber =
  type T = PhoneNumber of string

  let create (s: string) =
    if s = "" 
    then Error "Phone number must not be empty"
    else if Regex.IsMatch(s, @"^[+|0]\d+$") |> not
    then Error "Phone number has wrong format"
    else Ok (PhoneNumber s)

  let value (PhoneNumber e) = e

type Contact =
  {
    FirstName: FirstName.T
    LastName: LastName.T
    EmailAddress: EmailAddress.T
    PhoneNumber: PhoneNumber.T
  }

let createContact firstName lastName emailAddress phoneNumber = 
  {
    FirstName = firstName
    LastName = lastName
    EmailAddress = emailAddress
    PhoneNumber = phoneNumber
  }