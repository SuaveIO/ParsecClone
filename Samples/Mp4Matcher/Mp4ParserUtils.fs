﻿namespace Mp4Matcher

open ParsecClone
open ParsecClone.BinaryCombinator
open System
open System.Text

[<AutoOpen>]
module Mp4ParserUtils =

    let injector name f = f()//MiscUtils.time name f

    let private timeSince1904 = new DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc);

    let date : VideoParser<_> = bp.byte4 |>> (bp.toUInt32 >> Convert.ToDouble >> timeSince1904.AddSeconds)

    /// <summary>
    /// Returns a 4 byte sequence as an ascii string
    /// </summary>
    let stringId : VideoParser<_> = bp.byte4  |>> Encoding.ASCII.GetString

    /// <summary>
    /// Matches a string asn ascii byte sequence in the steam
    /// </summary>
    /// <param name="value"></param>
    let name (value:string) : VideoParser<_> = bp.matchBytes (Encoding.ASCII.GetBytes value) |>> Encoding.ASCII.GetString

    let atomSize : VideoParser<_> = bp.uint32

    let skipRemaining (start : uint32) consumed : VideoParser<_> =
        let skipAmount = ((int)start - consumed)
        if skipAmount > 0 then bp.skip skipAmount
        else preturn false

    /// <summary>
    /// Parser that sets the user state with the current stream position
    /// </summary>
    let trackStatePosition =
        statePosition >>= fun pos ->
        getUserState  >>= fun state ->
        setUserState { state with CurrentStatePosition = pos }

    /// <summary>
    /// Extracts an atom of the target name (if it exists)
    /// and keeps track in the user state what the starting position
    /// of this atom is
    /// </summary>
    /// <param name="id"></param>
    let atom id : VideoParser<_> =
        attempt (
            trackStatePosition >>= fun _ ->
            atomSize >>= fun size ->
            name id >>= fun name ->
            preturn
                {
                    Size = size
                    Name = name
                }
        ) //>>-- injector "attempt"


    /// <summary>
    /// Free atom parser
    /// </summary>
    let free : VideoParser<_> =
        atom "free"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id  |>> FREE

    /// <summary>
    /// Optional free atom parser
    /// </summary>
    let freeOpt : VideoParser<_> = opt free


    /// <summary>
    /// Consumes an atom if its name is a valid 4 letter lowercase ascii sequence
    /// </summary>
    let unknown : VideoParser<_> =
        trackStatePosition >>.
        atomSize           >>= fun size ->
        stringId           >>= fun name ->

        let intValues = name.ToCharArray() |> Array.map (int)
        let nonAscii = Array.exists(fun i -> i < 97 || i > 122) intValues

        if nonAscii then
            pzero
        else
            skipRemaining size 8 >>.
            preturn
                {
                    Size = size
                    Name = name
                }


    /// <summary>
    /// Runs the parser while the user state
    /// stream pos - start < size
    /// </summary>
    /// <param name="start"></param>
    /// <param name="size"></param>
    /// <param name="parser"></param>
    let takeIfAtomNotConsumed start expectedSize parser =
        let shouldConsume = fun (s:VideoState) ->
                                    let consumed = s.CurrentStatePosition - (int64)start
                                    consumed < (int64)expectedSize

        satisfyUserState shouldConsume parser // >>-- injector "satisfyUserrState"

    /// <summary>
    /// runs the parser many times until the
    /// target amount is consumed (making sure the atom has been fully consumed)
    /// </summary>
    /// <param name="name"></param>
    /// <param name="getParser"></param>
    let fullConsume name getParser =
        atom name >>= fun id ->
        getUserState >>= fun state ->

        let parser = getParser id

        takeIfAtomNotConsumed state.CurrentStatePosition id.Size parser |> many  >>-- injector "fullConsume"


    let versionAndFlags : VideoParser<_> =
        bp.byte1 |>> bp.byteToUInt  >>= fun version ->
        bp.byte3 |>> bp.toUInt24    >>= fun flags ->
        preturn
            {
                Version = version
                Flags = flags
            }


    let ``16.16`` =
        bp.uint32 >>= fun i ->
        preturn <| i / uint32(pown 2 16)

    let ``2.30`` =
        bp.uint32 >>= fun i ->
        preturn <| i / uint32(pown 2 30)

    let matrixRow =
        ``16.16``   >>= fun x1 ->
        ``16.16``   >>= fun x2 ->
        ``2.30``    >>= fun x3 ->
        preturn [|x1; x2; x3|]

    let matrix : VideoParser<_> =
        exactly 3 matrixRow >>|. List.toArray >>= fun rows ->

        preturn <| Array2D.init 3 3 (fun i j -> rows.[i].[j])