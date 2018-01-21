module ParsecClone.Tests.CsvtringUnitTests

open Expecto
open Expecto.Flip
open ParsecClone
open ParsecClone.StringCombinator
open StringMatchers.CsvSample


let testEmptyWhiteSpace() =
  let csv = makeStringStream ""

  let result = test csv ws

  result |> Expect.equal "equal" ""

let testWhiteSpace() =
  let csv = makeStringStream " "

  let result = test csv ws

  result |> Expect.equal "equal" " "

let testElement() =
  let csv = makeStringStream "some text"

  let result = test csv csvElement

  result |> Expect.equal "equal" "some text"

let testElements() =
    let csv = makeStringStream "some text,"

    let result = test csv csvElement

    result |> Expect.equal "equal" ("some text")

let testTwoElement() =
  let csv = makeStringStream "some text, text two"

  let result = test csv elements

  result |> Expect.equal "equal" (["some text";"text two"] |> List.map Some)

let testTwoLines() =
  let t = @"a, b
c, d"

  let csv = makeStringStream t

  let result = test csv lines

  result |> Expect.equal "equal" [["a";"b"] |> List.map Some;
                          ["c";"d"] |> List.map Some]

let testEscaped() =
  let t = @"\,"

  let csv = makeStringStream t

  let result = test csv escapedChar

  result |> Expect.equal "equal" ","

let testLiteral() =
  let t = "\"foo\""

  let csv = makeStringStream t

  let result = test csv literal

  result |> Expect.equal "equal" "foo"

let testLiteral2() =
  let t = "a\,b"

  let csv = makeStringStream t

  let result = test csv normalAndEscaped

  result |> Expect.equal "equal" "a,b"

let testUnEscaped1() =
  let t = "a,b"

  let csv = makeStringStream t

  let result = test csv normalAndEscaped

  result |> Expect.equal "equal" "a"

let testCsvWithQuotes1() =
  let t = "\"cd,fg\""

  let csv = makeStringStream t

  let result = test csv lines

  result |> Expect.equal "equal" [["cd,fg"] |> List.map Some]

let testEmpties() =
  let t = ",,,"

  let csv = makeStringStream t

  let result = test csv lines

  result |> Expect.equal "equal" [[Some "";Some "";Some "";Some ""]]

let testCsvWithQuotes2() =
  let t = "a,\"b 1.\\\",\"cd,fg\"
a,b,\\\", \"cd,fg\",,"

  let csv = makeStringStream t

  let result = test csv lines |> List.toArray

  result.[0] |> Expect.equal "equal" (["a";"b 1.\\";"cd,fg"] |> List.map Some)
  result.[1] |> Expect.equal "equal" ([Some("a");Some("b");Some("\"");Some("cd,fg");Some "";Some ""])

let testCsvWithOptionalElements() =
  let t = ",,"

  let csv = makeStringStream t

  let result = test csv lines |> List.toArray

  result.[0] |> Expect.equal "equal" ([Some ""; Some ""; Some ""])



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


let testCsvWithEscapedNewlines() =
  let t = "a\\nb"

  let csv = makeStringStream t

  let result = test csv lines |> List.toArray

  result.[0] |> Expect.equal "equal" (["a\nb"] |> List.map Some)

let testCsvWithNewlinesInQuotes() =
  let t = @"""a

b"""

  let csv = makeStringStream t

  let result = test csv lines

  result |> Expect.equal "equal" [[@"a

b"] |> List.map Some]


let testReadmeExample1 () =
  let t = "foo\,,,bar,baz\\\"
faisal rules!"

  let csv = makeStringStream t

  let result = test csv lines |> List.toArray

  result.[0] |> Expect.equal "equal" ([Some("foo,"); Some ""; Some("bar"); Some("baz\"")])

let testDoubleQuotes () =
  let t = "\"a\" b def \"foo\","

  let csv = makeStringStream t

  let result = test csv lines |> List.toArray

  List.ofArray result
    |> Expect.equal "equal" [ [Some(@"a b def foo"); Some ""] ]

let testEofMissing () =
  let t = "\"foo,\",bar,baz"

  let csv = makeStringStream t

  Expect.throws "Should throw" <| fun () ->
    let _ = test csv (csvElement .>> eof)
    ()


[<Tests>]
let tests =
  testList "csv sample" [
    testCase "empty whitespace" testEmptyWhiteSpace
    testCase "white space" testWhiteSpace
    testCase "element" testElement
    testCase "elements" testElements
    testCase "two elements" testTwoElement
    testCase "two lines" testTwoLines
    testCase "escaped" testEscaped
    testCase "literal (1)" testLiteral
    testCase "literal (2)" testLiteral2
    testCase "unescaped" testUnEscaped1
    testCase "csv with quotes (1)" testCsvWithQuotes1
    testCase "csv with quotes (2)" testCsvWithQuotes2
    testCase "empties" testEmpties
    testCase "csv with optional elements" testCsvWithOptionalElements
    testCase "all" testAll
    testCase "csv with escaped newlines" testCsvWithEscapedNewlines
    testCase "csv with newlines in quotes" testCsvWithNewlinesInQuotes
    testCase "readme example 1" testReadmeExample1
    testCase "double quotes" testDoubleQuotes
    testCase "eof missing" testEofMissing
  ]