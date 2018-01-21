module ParsecClone.Tests.Mp4ParserTests

open System.IO
open Expecto
open Expecto.Flip
open Mp4Matcher
open ParsecClone.CombinatorBase

let audioStts result =
  let getAudioMetadata src = maybe {
    let extractAudio (trak:Trak) = maybe {
      let! mdia = trak.Mdia
      let! minf = mdia.Minf
      let! header =  minf.MediaTypeHeader
      let! audio = header.Audio
      let! stbl = minf.Stbl
      let! stts = stbl.Stts
      return stts
    }

    let! mov = src.Mov
    let tracks = mov.Traks

    return List.map extractAudio tracks |> List.filter Option.isSome |> List.map Option.get
  }

  match getAudioMetadata result with
  | Some(stts::_) -> Some(stts)
  | _ -> None

let moovFtypTest() =

  use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

  let parserStream = mp4Stream f

  let result = test parserStream ftyp

  let expected =
    { MajorBrand = "mp42"
      MinorVersion = 0x0u
      Brands =
        Some [
          "mp42"
          "isom"
          "avc1"
        ]
    }

  result |> Expect.equal "equal" (FTYP expected)

let testUnknown() =

  use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

  let parserStream = mp4Stream f

  let result = test parserStream unknown

  result.Name |> Expect.equal "equal" "ftyp"

let testUnknownFailure() =

  use f = new MemoryStream([|0x00;0x00;0x00;0x00;0xD0;0xFF;0x01;0x01|] |> Array.map byte)

  let parserStream = mp4Stream f

  let result = test parserStream unknown

  result.Name |> Expect.equal "equal" "ftyp"

let expectedMatchOnName() =

    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = mp4Stream f

    let _ = test parserStream (mvhd >>. ftyp)

    ()

let matchFtypAndMoov() =

  use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

  let parserStream = mp4Stream f

  let result = test parserStream video

  result.Length |> Expect.equal "equal" 3

  let audioStts = audioStts (result |> rootListToRecord)

  (Option.get audioStts).SampleTimes.[0].SampleDuration |> Expect.equal "equal" (uint32 1025)


let convertToRecordTest1() =

  use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

  let parserStream = mp4Stream f

  let result = test parserStream video

  let record = rootListToRecord result

  ()

let matchOptFtypAndMoov() =

  use f = new FileStream(@"NoFtyp.m4v", FileMode.Open)

  let parserStream = mp4Stream f

  let result = test parserStream video

  result.Length |> Expect.equal "equal" 2

  let audioStts = audioStts (result |> rootListToRecord)

  (Option.get audioStts).SampleTimes.[0].SampleDuration |> Expect.equal "equal" (uint32 1025)

  (Option.get audioStts).SampleTimes.[1].SampleDuration |> Expect.equal "equal" (uint32 661)


let sampleFromAppl1() =
  use f = new FileStream(@"sample_iPod.m4v", FileMode.Open)

  let parserStream = mp4Stream (new BufferedStream(f))

  let result = test parserStream video

  // even though it actually has 5 atoms at the root (including 2 free's)
  // the sub atoms have optionally consumed the free's and discarded it. since
  // we don't care about free's this is OK, so the length is actually 1 less

  result.Length |> Expect.equal "equal" 4

let sampleFromAppl2() =
  use f = new FileStream(@"sample_mpeg4.mp4", FileMode.Open)

  let parserStream = mp4Stream (new BufferedStream(f))

  let result = test parserStream video

  // even though it actually has 5 atoms at the root (including 2 free's)
  // the sub atoms have optionally consumed the free's and discarded it. since
  // we don't care about free's this is OK, so the length is actually 1 less

  result.Length |> Expect.equal "equal" 4

let madeByFfmpeg() =

  use f = new FileStream(@"ffmpegMade.m4v", FileMode.Open)

  let parserStream = mp4Stream f

  let result = test parserStream video

  printf "%s" <| result.ToString()

  result.Length |> Expect.equal "equal" 4

[<Tests>]
let tests =
  testList "mp4" [
    testCase "moov ftyp" moovFtypTest
    testCase "unknown" testUnknown
    testCase "unknown failure" <| fun () ->
      Expect.throws "Should throw" testUnknownFailure
    testCase "match on name" <| fun () ->
      Expect.throws "Should throw" expectedMatchOnName
    testCase "match ftyp and moov" matchFtypAndMoov
    testCase "convert to record" convertToRecordTest1
    testCase "match opt ftyp and moov" matchOptFtypAndMoov
    testCase "sample from appl (1)" sampleFromAppl1
    testCase "sample from appl (2)" sampleFromAppl2
    testCase "made by FFMPEG" madeByFfmpeg
  ]