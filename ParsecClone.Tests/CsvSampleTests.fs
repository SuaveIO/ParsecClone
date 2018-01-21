module ParsecClone.Tests.CsvtringUnitTests

open System
open Expecto
open Expecto.Flip

open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase
open StringMatchers.CsvSample


[<Test>]
let testEmptyWhiteSpace() =
    let csv = makeStringStream ""

    let result = test csv ws

    result |> Expect.equal "equal" ""

[<Test>]
let testWhiteSpace() =
    let csv = makeStringStream " "

    let result = test csv ws

    result |> Expect.equal "equal" " "

[<Test>]
let testElement() =
    let csv = makeStringStream "some text"

    let result = test csv csvElement

    result |> Expect.equal "equal" "some text"

[<Test>]
let testElements() =
    let csv = makeStringStream "some text,"

    let result = test csv csvElement

    result |> Expect.equal "equal" ("some text")

[<Test>]
let testTwoElement() =
    let csv = makeStringStream "some text, text two"

    let result = test csv elements

    result |> Expect.equal "equal" (["some text";"text two"] |> List.map Some)

[<Test>]
let testTwoLines() =
    let t = @"a, b
c, d"

    let csv = makeStringStream t

    let result = test csv lines

    result |> Expect.equal "equal" [["a";"b"] |> List.map Some;
                            ["c";"d"] |> List.map Some]

[<Test>]
let testEscaped() =
    let t = @"\,"

    let csv = makeStringStream t

    let result = test csv escapedChar

    result |> Expect.equal "equal" ","

[<Test>]
let testLiteral() =
    let t = "\"foo\""

    let csv = makeStringStream t

    let result = test csv literal

    result |> Expect.equal "equal" "foo"

[<Test>]
let testLiteral2() =
    let t = "a\,b"

    let csv = makeStringStream t

    let result = test csv normalAndEscaped

    result |> Expect.equal "equal" "a,b"

[<Test>]
let testUnEscaped1() =
    let t = "a,b"

    let csv = makeStringStream t

    let result = test csv normalAndEscaped

    result |> Expect.equal "equal" "a"

[<Test>]
let testCsvWithQuotes1() =
    let t = "\"cd,fg\""

    let csv = makeStringStream t

    let result = test csv lines

    result |> Expect.equal "equal" [["cd,fg"] |> List.map Some]

[<Test>]
let testEmpties() =
    let t = ",,,"

    let csv = makeStringStream t

    let result = test csv lines

    result |> Expect.equal "equal" [[Some "";Some "";Some "";Some ""]]

[<Test>]
let testCsvWithQuotes2() =
    let t = "a,\"b 1.\\\",\"cd,fg\"
a,b,\\\", \"cd,fg\",,"

    let csv = makeStringStream t

    let result = test csv lines |> List.toArray

    result.[0] |> Expect.equal "equal" (["a";"b 1.\\";"cd,fg"] |> List.map Some)
    result.[1] |> Expect.equal "equal" ([Some("a");Some("b");Some("\"");Some("cd,fg");Some "";Some ""])

[<Test>]
let testCsvWithOptionalElements() =
    let t = ",,"

    let csv = makeStringStream t

    let result = test csv lines |> List.toArray

    result.[0] |> Expect.equal "equal" ([Some ""; Some ""; Some ""])



[<Test>]
let testAll() =
    let t = @"This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"""

    let csv = makeStringStream t

    let result = test csv lines

    List.length result |> Expect.equal "equal" 11


[<Test>]
let testCsvWithEscapedNewlines() =
    let t = "a\\nb"

    let csv = makeStringStream t

    let result = test csv lines |> List.toArray

    result.[0] |> Expect.equal "equal" (["a\nb"] |> List.map Some)

[<Test>]
let testCsvWithNewlinesInQuotes() =
    let t = @"""a

b"""

    let csv = makeStringStream t

    let result = test csv lines

    result |> Expect.equal "equal" [[@"a

b"] |> List.map Some]


[<Test>]
let testReadmeExample1 () =
    let t = "foo\,,,bar,baz\\\"
faisal rules!"

    let csv = makeStringStream t

    let result = test csv lines |> List.toArray

    result.[0] |> Expect.equal "equal" ([Some("foo,"); Some ""; Some("bar"); Some("baz\"")])

[<Test>]
let testDoubleQuotes () =
    let t = "\"a\" b def \"foo\","

    let csv = makeStringStream t

    let result = test csv lines |> List.toArray

    result |> Expect.equal "equal" [[Some(@"a b def foo"); Some ""]]

[<Test>]
[<ExpectedException>]
let testEofMissing () =
    let t = "\"foo,\",bar,baz"

    let csv = makeStringStream t

    let r = test csv (csvElement .>> eof)

    r |> Expect.equal "equal" true

