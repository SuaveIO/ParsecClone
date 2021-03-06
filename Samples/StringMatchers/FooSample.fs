﻿namespace StringMatchers

open ParsecClone
open ParsecClone.StringCombinator

module FooSample =
    type FooParser<'Return> = Parser<'Return, string, string, unit>

    type Band =
        | FooFighter

    let foo = matchStr "foo"

    let fighter = matchStr "fighter"

    let fRegex : FooParser<_> = regexStr "fo{2}f"

    let fooFightersWithSpaces : FooParser<_> = many (whitespaces <|> chars)

    let band : FooParser<_> = foo >>. fighter |>>% FooFighter

    let fooString : FooParser<_> = foo .>> fighter
    let fighterString : FooParser<_>  = foo >>. fighter
    let fighterTuples : FooParser<_> = foo .>>. fighter

    let validFooChar : FooParser<_> = anyOf matchStr ["f";"o";"i";"g";"h";"t";"e";"r";"s";" "]

    let allFooCharacters : FooParser<_> = many validFooChar

    let err : FooParser<_> =
        fighter >>=? fun v ->
                     foo

    let errAttempt = matchStr "fo"

    let parseWithErrorAttempt : FooParser<_> = choice[attempt (errAttempt >>. errAttempt) |>>% FooFighter;band]

    let manyFoo : FooParser<_> = many foo

    let opts : FooParser<_> = fighter <|> foo

    let optsC : FooParser<_> = choice[fighter;foo]

