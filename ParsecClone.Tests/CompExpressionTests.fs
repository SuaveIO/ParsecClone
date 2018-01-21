module ParsecClone.Tests.CompExpressionTests

open Expecto
open Expecto.Flip
open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase

let testExpression() =
    let state = makeStringStream "this is a test"

    let parser = parse {
      let! _ = matchStr "this"
      let! _ = ws
      let! _ = matchStr "is a"
      let! _ = ws
      return! matchStr "test"
    }

    let result = test state parser

    result |> Expect.equal "equal" "test"

[<Tests>]
let tests = testCase "expression" testExpression