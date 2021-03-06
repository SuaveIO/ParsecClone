﻿module ParsecClone.Tests.BinaryUnitTests

open System
open Expecto
open Expecto.Flip
open System.IO
open ParsecClone.BinaryCombinator
open ParsecClone

let bp = new BinParser<_>(Array.rev)

let byte1 = bp.byte1
let byte2 = bp.byte2
let byte3 = bp.byte3
let byte4 = bp.byte4
let byteN = bp.byteN
let int32 = bp.int32
let int16 = bp.int16
let intB = bp.intB
let uint16 = bp.uint16


let binTest1() =
  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let parser = byte1

  let result = test parserStream parser

  result |> Expect.equal "Should be zero" 0uy

let inline byteListToArr arr =
  arr |> Seq.take 4 |> Seq.toArray |> Array.map byte

let binTest2() =
  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let parser = manyN 4 byte1

  let result = test parserStream parser

  byteListToArr result |> Expect.equal "equal" (Array.take 4 bytes)

let binTest3() =
  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let parser = manyN 2 byte4

  let result = test parserStream parser

  result |> Expect.equal "equal" [[|0uy;1uy;2uy;3uy|];[|4uy;5uy;6uy;7uy|]]

let binTest4() =
  let p = new BinParser<_>(id)

  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let result = test parserStream p.int32

  result |> Expect.equal "equal" 50462976


let binaryWithBacktracker() =
  let p = new BinParser<_>(id)

  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let failureParse =  manyN 10 p.int32

  let backtrackWithFail = p.int32 >>=? fun b1 ->
                          failureParse >>= fun b2 ->
                          preturn b1

  let consume1 = backtrackWithFail .>>. byte1

  let result = test parserStream consume1

  result |> Expect.equal "equal" (50462976, byte(4))

let ``test backtracker with attempt operator``() =
  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let shouldFail = int32 >>= fun b1 ->
                             manyN 10 int32 >>= fun b2 ->
                             preturn b1

  let consume1 = choice[attempt shouldFail; intB]

  let result = test parserStream consume1

  result |> Expect.equal "equal" 0

let takeTillTest() =
  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let takenLower = takeTill (fun i -> i >= byte(4)) byte1

  let result = test parserStream takenLower

  Array.ofList result |> Expect.equal "equal" [|0uy;1uy;2uy;3uy|]

let takeTillTest2() =
  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let stream = new MemoryStream(bytes)

  let parserStream = makeBinStream(stream)

  let takeUpper = takeTill (fun i -> i >= byte(4)) byte1 >>. byteN 5

  let result = test parserStream takeUpper

  result |> Expect.equal "equal" ([|4;5;6;7;8|] |> Array.map byte)


let takeWhileTest() =
  let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

  let parserStream = makeBinStream(new MemoryStream(bytes))

  let takeUpper = takeWhile (fun i -> i < byte(4)) byte1 >>. byteN 5

  let result = test parserStream takeUpper

  result |> Expect.equal "equal" ([|4;5;6;7;8|] |> Array.map byte)

let endianessTest() =
  let bytes = [|0x11;0x7B;0;0;0;0;0;0|] |> Array.map byte

  let parserStream = makeBinStream(new MemoryStream(bytes))

  let result = test parserStream uint16

  result |> Expect.equal "equal" 4475us


let endianessTest2() =
  let bytes = [|0xF1;0x7B;0;0;0;0;0;0|] |> Array.map byte

  let parserStream = makeBinStream(new MemoryStream(bytes))

  let result = test parserStream int16

  result |> Expect.equal "equal" -3717s

let bitParserTest() =
  let bytes = [|0xF0;0x01|] |> Array.map byte

  let parserStream = makeBinStream(new MemoryStream(bytes))

  let bitToBool = bp.bitsN 4 |>> bp.bitsToInt

  let bitP = bp.makeBitP (bp.byteN 1) bitToBool

  let result = test parserStream (bitP .>> bp.byte1 .>> eof)

  result |> Expect.equal "equal" 15

let selectBitTest() =
  let bytes = [|0xF0;0x01|] |> Array.map byte

  let parserStream = makeBinStream(new MemoryStream(bytes))

  let selectLastBit = bp.bitN 16

  let bitP = bp.makeBitP (byteN 2) selectLastBit

  let result = test parserStream (bitP .>> eof)

  result |> Expect.equal "equal" One

let testConsumingBitsError() =
  let bytes = [|0xF0|] |> Array.map byte

  let parserStream = makeBinStream(new MemoryStream(bytes))

  // exception should happen since we consumed one bit
  // then tried to consume 8 more, but the underlying byte array
  // only had 8 total bits
  let selectLastBit = bp.bit1 >>. bp.bit8

  let bitP = bp.makeBitP (byteN 1) selectLastBit

  let result = test parserStream (bitP .>> eof)

  result |> Expect.equal "equal" Zero

let testConsumingBits() =
  let bytes = [|0x01|] |> Array.map byte

  let parserStream = makeBinStream(new MemoryStream(bytes))

  let selectLastBit = bp.bit1 >>= fun one ->
                      bp.bit1 >>= fun two ->
                      bp.bit1 >>= fun three ->
                      bp.bit1 >>= fun four ->
                      bp.bit1 >>= fun five ->
                      bp.bit1 >>= fun six ->
                      bp.bit1 >>= fun seven ->
                      bp.bit1 >>= fun eight ->
                      preturn [|one;two;three;four;five;six;seven;eight|]

  let bitP = bp.makeBitP (byteN 1) (selectLastBit .>> eof) // the eof here is to make sure we read all the bits

  let result = test parserStream (bitP .>> eof) // make sure we read all the bytes

  result |> Expect.equal "equal" <| bytesToBits [|byte(0x01)|]

let testApplyManyBits() =
  let bytes = Array.init 10 (fun i -> byte(0x01))

  let parserStream = makeBinStream(new MemoryStream(bytes))

  let selectLastBit = bp.bit1 >>= fun one ->
                      bp.bit1 >>= fun two ->
                      bp.bit1 >>= fun three ->
                      bp.bit1 >>= fun four ->
                      bp.bit1 >>= fun five ->
                      bp.bit1 >>= fun six ->
                      bp.bit1 >>= fun seven ->
                      bp.bit1 >>= fun eight ->
                      preturn [|one;two;three;four;five;six;seven;eight|]

  let bitP = bp.makeBitP (byteN 1) selectLastBit

  let result = test parserStream (many bitP .>> eof)

  let target = [0..Array.length bytes - 1] |> List.map (fun _ -> bytesToBits <| bytes.[0..0])

  result |> Expect.equal "equal" target


let lookaheadAndSeek () =
  let writeTestFile name =
      use f = File.OpenWrite(name)
      use bw = new BinaryWriter(f)

      let value = 55
      let offset = 32

      bw.BaseStream.Seek(int64 offset, SeekOrigin.Begin) |> ignore

      bw.Write(BitConverter.GetBytes(value))

      bw.BaseStream.Seek(int64 60, SeekOrigin.Begin) |> ignore
      bw.Write(BitConverter.GetBytes(offset))

      name

  let bp = new BinParser<_>(id)

  let testFile = Path.GetRandomFileName()

  let fs = File.OpenRead(writeTestFile testFile)

  let binstream = makeBinStream fs

  let findOffset = bp.skip 60 >>. bp.int32
  let parser =
      lookahead findOffset >>= fun offset ->
      bp.skip offset >>.
      bp.int32

  let result = parser |> test binstream

  result |> Expect.equal "equal" 55

  fs.Dispose()

  File.Delete testFile


let lookaheadAndSeekRaw () =
  let writeTestFile name =
      use f = File.OpenWrite(name)
      use bw = new BinaryWriter(f)

      let value = 55
      let offset = 32

      bw.BaseStream.Seek(int64 offset, SeekOrigin.Begin) |> ignore

      bw.Write(BitConverter.GetBytes(value))

      bw.BaseStream.Seek(int64 60, SeekOrigin.Begin) |> ignore
      bw.Write(BitConverter.GetBytes(offset))

      name

  let bp = new BinParser<_>(id)

  let testFile = Path.GetRandomFileName()

  let fs = File.OpenRead(writeTestFile testFile)

  let binstream = makeBinStream fs

  let findOffset = bp.skip 60 >>. bp.int32
  let parser =
      findOffset >>= fun offset ->
      bp.seekTo offset >>.
      bp.int32

  let result = parser |> test binstream

  result |> Expect.equal "equal" 55

  fs.Dispose()

  File.Delete testFile


let testManyZeroOrMoreMatches () =
  let (bp: BinParser<unit>) = new BinParser<_> (id)

  let pByteZero = bp.matchBytes [| 00uy |] |>> (fun _ -> 00uy)

  let pByteOne = bp.matchBytes [| 01uy |] |>> (fun _ -> 01uy)

  let parser = parse {
    let! zeros = many pByteZero
    let! ones = many pByteOne
    return zeros, ones
  }

  let bytes = [| 01uy; 02uy |]

  let s = new MemoryStream (bytes) |> makeBinStream

  let result = test s parser

  result |> Expect.equal "equal" (([]: byte list), [1uy])

let testNegativeInts () =
  let (bp: BinParser<unit>) = new BinParser<_> (id)
  let bytes = [| 0xffuy; 0xffuy |]
  let s = new MemoryStream (bytes) |> makeBinStream
  test s bp.intB |> Expect.equal "equal" (-1)
  test s bp.uintB |> Expect.equal "equal" (255u)


[<Tests>]
let tests =
  testList "binary unit" [
    testCase "bin test 1" binTest1
    testCase "bin test 2" binTest2
    testCase "bin test 3" binTest3
    testCase "bin test 4" binTest4
    testCase "binary with backtracker" binaryWithBacktracker
    testCase "backtracker with attempt operator" ``test backtracker with attempt operator``
    testCase "take till" takeTillTest
    testCase "take till (2)" takeTillTest2
    testCase "take while" takeWhileTest
    testCase "endianess" endianessTest
    testCase "endianess (2)" endianessTest2
    testCase "bit parser" bitParserTest
    testCase "select bit" selectBitTest
    testCase "consuming bits error" <| fun () ->
      Expect.throws "Should throw exn" testConsumingBitsError
    testCase "consuming bits" testConsumingBits
    testCase "apply many bits" testApplyManyBits
    testCase "lookahead and seek" lookaheadAndSeek
    testCase "lookahead and seek raw" lookaheadAndSeekRaw
    testCase "many zero or more matches" testManyZeroOrMoreMatches
    testCase "negative ints" testNegativeInts
  ]