﻿module ParsecClone.Tests.CompExpressionTests

open Expecto.Flip
open Expecto
open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase

[<Test>]
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