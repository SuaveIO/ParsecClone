module ParsecClone.Tests.FooStringUnitTests

open Expecto
open Expecto.Flip
open ParsecClone
open ParsecClone.StringCombinator
open StringMatchers.FooSample

let shortCircuitOr () =
  let target = makeStringStream "fab"

  let x = choice[matchStr "f"; matchStr "a"; matchStr "b"]

  let band = test target x

  () |> Expect.equal "equal" ()

let preturnTest () =
  let target = makeStringStream "foofighters"
  let _ = test target band
  ()

let manyTest () =
  let manyFooStr = test (makeStringStream "foofoofoofoofob") manyFoo

  List.length manyFooStr
    |> Expect.equal "Has 4 foos" 4

let fooString () =
  let target = makeStringStream "foofighters"

  let fString = test target fooString

  fString
    |> Expect.equal "Shold eq foo" "foo"

let fightString () =
  let target = makeStringStream "foofighters"

  let fightString = test target fighterString

  fightString
    |> Expect.equal "Should be a fighter" "fighter"

let testTuples () =
  let target = makeStringStream "foofighters"

  let (foo, fighters) = test target fighterTuples

  fighters
    |> Expect.equal "Should be a fighter" "fighter"


let options () =
  let target = makeStringStream "foofighters"

  test target opts
    |> Expect.equal "Should be foo" "foo"

  test target optsC
    |> Expect.equal "Should be foo" "foo"

let manyOptions () =
  let target = makeStringStream "foofighters" |> toInterface

  test target (many opts)
    |> Expect.equal "Is an array of foo and fighter" ["foo";"fighter"]

  test target (many optsC)
    |> Expect.equal "Is an array of foo and fighter" ["foo";"fighter"]
let regex () =
  let target = makeStringStream "foofighters"

  test target fRegex
    |> Expect.equal "Parsed a foof" "foof"

let regexes () =
  let target = makeStringStream "      foofighters           foofighters"

  test target fooFightersWithSpaces
    |> List.length
    |> Expect.equal "Should be four results" 4

let anyOfChars () =
  let target = makeStringStream "      foofighters           foofighters" |> toInterface

  let result = test target allFooCharacters |> List.fold (+) ""

  result
    |> Expect.equal "Target state eq result" target.state

let newline () =
  let fullNewline = makeStringStream "\r\n"  |> toInterface
  let carriageReturn = makeStringStream "\r" |> toInterface
  let newLine = makeStringStream "\n"  |> toInterface
  let nl = @"
"
  let newLine2 = makeStringStream nl |> toInterface

  test fullNewline newline
    |> Expect.equal "Parses full newline" fullNewline.state
  test carriageReturn newline
    |> Expect.equal "Parses linefeed" carriageReturn.state
  test newLine newline
    |> Expect.equal "Parses newline (1)" newLine.state
  test newLine2 newline
    |> Expect.equal "Parses newlinew (2)" newLine2.state

let attemptTest () =
  let target = makeStringStream "foofighters"

  let _ = test target parseWithErrorAttempt
  ()

let manyTillTest () =
  let target = makeStringStream "abc abc def abc"

  let abc = matchStr "abc" .>> ws

  let def = matchStr "def"

  let line = (manyTill abc def .>> ws) .>>. abc .>> eof

  let result = test target line

  result |> Expect.equal "equal" (["abc";"abc"],"abc")

let manyTillOneOrMore () =
  let target = makeStringStream "x abc def abc"

  let abc = matchStr "abc" .>> ws

  let def = matchStr "def"

  let line = (manyTill1 abc def .>> ws) .>>. abc .>> eof

  let result = test target line

  result |> Expect.equal "equal" (["abc";"abc"],"abc")

let lookaheadTest () =
  let target = makeStringStream "abc abc def abc" |> toInterface

  let abc =
    lookahead (matchStr "abc" .>> ws) >>= fun r ->
    if r = "abc" then preturn "found"
    else preturn "not found"

  match abc target with
  | Some(m), state ->
    m
      |> Expect.equal "equal" "found"
    state.state
      |> Expect.equal "equal" target.state

  | None, _ ->
    false
      |> Expect.equal "equal" true

let many1TestFail () =
  let target = makeStringStream "abc abc def abc" |> toInterface

  let foo = matchStr "foo"

  let manyFoo = many1 foo

  test target manyFoo |> ignore

let many1Test () =
  let target = makeStringStream "abc abc def abc" |> toInterface

  let abc = ws >>. matchStr "abc"

  let manyAbc = many1 abc

  test target manyAbc |> Expect.equal "equal" ["abc";"abc"]

let testForwardingRefP() =
  let target = makeStringStream "{abc}" |> toInterface

  let abc = matchStr "abc"

  let impl, fwd = createParserForwardedToRef()

  fwd := between (matchStr "{") abc (matchStr "}")

  let result = test target impl

  result |> Expect.equal "equal" "abc"

let testForwardingRefPRecursive() =
  let target = makeStringStream "{a{a{a{a{a}}}}}"


  let impl, fwd = createParserForwardedToRef()

  let a  = matchStr "a"
  let lB = matchStr "{"
  let rB = matchStr "}"

  let brak = between lB (a .>> opt impl) rB

  fwd := brak

  let result = test target (impl .>> eof)

  result |> Expect.equal "equal" "a"

let reprocessTest() =
    let target = makeStringStream "abc" |> toInterface

    let abc = matchStr "abc"

    let a = matchStr "a" >>= fun a ->
            matchStr "b" >>= fun b ->
            matchStr "c" >>= fun c -> preturn (a + b + c + "foo")

    let elevator = fun i s -> makeStringStream(i) |> toInterface

    let r = reproc elevator (abc .>> eof) a

    let result = test target r

    result |> Expect.equal "equal" "abcfoo"

let stringLiteralTest() =
    let source = "Ex\\\"loremIpsum\\\" foo \\\"second string\\\" "

    let target = sprintf "\"%s\"" source |> makeStringStream

    let result = test target (quotedStringLiteral |> between2 (matchStr "\""))

    result |> Expect.equal "equal" source

let stringLiteralExTest() =

    let source = "a\,b\\n\r\\t,notmatched" |> makeStringStream

    let delim = ","

    let p = stringLiteral delim "\\"

    let result = test source (many (p |> sepBy <| (matchStr delim)))

    result |> Expect.equal "equal" ["a\,b\\n\r\\t"; "notmatched"]


let stringLiteralExTest2() =

    let source = "\t" |> makeStringStream

    let delim = ","

    let p = stringLiteral delim "\\"

    let result = test source (many (p |> sepBy <| (matchStr delim)))

    result |> Expect.equal "equal" ["\t"]

let testFloat() =
    let random = new System.Random()

    for i in [0..100] do
        let randomDouble = random.NextDouble()
        let randomInt = random.Next()

        let doubleStream = randomDouble.ToString().Substring(0, 15) |> makeStringStream
        let intStream = randomInt.ToString() |> makeStringStream

        let r1 = test doubleStream pfloat
        r1 |> Expect.floatClose "close/equal" Accuracy.veryHigh randomDouble

        let r2 = test intStream pfloat
        r2 |> Expect.equal "equal" (System.Convert.ToDouble randomInt)

        let r3 = test intStream pint
        r3 |> Expect.equal "equal" randomInt

[<Tests>]
let tests =
  testList "foo string" [
    testCase "short circuit or" shortCircuitOr
    testCase "preturn" preturnTest
    testCase "many" manyTest
    testCase "foo string" fooString
    testCase "fight" fightString
    testCase "tuples" testTuples
    testCase "options" options
    testCase "many options" manyOptions
    testCase "regex" regex
    testCase "regexes" regexes
    testCase "any of chars" anyOfChars
    testCase "newline" newline
    testCase "attempt" attemptTest
    testCase "many till" manyTillTest
    testCase "many till one or more" <| fun () ->
      Expect.throws "Should throw something" manyTillOneOrMore
    testCase "lookahead" lookaheadTest
    testCase "many1 failure" <| fun () ->
      Expect.throws "Should throw" many1TestFail
    testCase "many1" many1Test
    testCase "forwarding ref P" testForwardingRefP
    testCase "forwarding ref P recursive" testForwardingRefPRecursive
    testCase "reprocess" reprocessTest
    testCase "string literal" stringLiteralTest
    testCase "string literal ex (1)" stringLiteralExTest
    testCase "string literal ex (2)" stringLiteralExTest2
    testCase "float" testFloat
  ]