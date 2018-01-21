module ParsecClone.Tests.UserStateTests

open Expecto
open Expecto.Flip
open ParsecClone.CombinatorBase
open ParsecClone.StringCombinator

type userState = { Name: string }

let testUserState() =

    let state = new StringStreamP<userState>("foobar", { Name = "start!" })

    let st = matchStr "foo" >>= fun x ->
             setUserState  { Name = x }

    let result = st >>. getUserState

    let r = test state result

    r |> Expect.equal "equal" { Name = "foo" }

[<Tests>]
let tests = testCase "user state" testUserState