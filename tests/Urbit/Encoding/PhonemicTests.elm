module Urbit.Encoding.PhonemicTests exposing (..)

import BigInt
import Expect
import Fuzz
import Random
import Test exposing (Test, describe, fuzz, test)
import Urbit.Encoding.Atom as Atom
import Urbit.Encoding.Phonemic as Phonemic


patq : Test
patq =
    describe "@q"
        [ fuzz fuzzUInt "Encode/decode equivalence" <|
            \int ->
                int
                    |> Atom.fromInt
                    |> Maybe.andThen
                        (Phonemic.toPatq
                            >> Phonemic.fromPatq
                            >> Result.toMaybe
                        )
                    |> Maybe.andThen
                        (Atom.toBigInt >> BigInt.toString >> String.toInt)
                    |> Expect.equal (Just int)
        , test "Cannot encode negative ints" <|
            \() ->
                -1
                    |> Atom.fromInt
                    |> Maybe.map Phonemic.toPatq
                    |> Expect.equal Nothing
        , test "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet" <|
            \() ->
                "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet"
                    |> Phonemic.fromPatq
                    |> Result.map (Atom.toBigInt >> BigInt.toString)
                    |> Expect.equal
                        (Ok "5895839060854263750826948981758590799")
        , test "5895839060854263750826948981758590799" <|
            \() ->
                "5895839060854263750826948981758590799"
                    |> BigInt.fromIntString
                    |> Maybe.andThen Atom.fromBigInt
                    |> Maybe.map Phonemic.toPatq
                    |> Expect.equal
                        (Just "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet")
        , test "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
            (\() ->
                "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
                    |> Phonemic.fromPatq
                    |> Result.map (Atom.toBigInt >> BigInt.toString)
                    |> Expect.err
            )
        ]


patp : Test
patp =
    describe "@p"
        [ fuzz fuzzUInt "Encode/decode equivalence" <|
            \int ->
                int
                    |> Atom.fromInt
                    |> Maybe.andThen
                        (Phonemic.toPatp
                            >> Phonemic.fromPatp
                            >> Result.toMaybe
                        )
                    |> Maybe.andThen
                        (Atom.toBigInt >> BigInt.toString >> String.toInt)
                    |> Expect.equal (Just int)
        , test "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
            (\() ->
                "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
                    |> Phonemic.fromPatp
                    |> Result.map (Atom.toBigInt >> BigInt.toString)
                    |> Expect.equal
                        (Ok "5895839060854263750826948981758590799")
            )
        , test "5895839060854263750826948981758590799" <|
            \() ->
                "5895839060854263750826948981758590799"
                    |> BigInt.fromIntString
                    |> Maybe.andThen Atom.fromBigInt
                    |> Maybe.map Phonemic.toPatp
                    |> Expect.equal
                        (Just "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet")
        , test "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet" <|
            \() ->
                "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet"
                    |> Phonemic.fromPatp
                    |> Result.map (Atom.toBigInt >> BigInt.toString)
                    |> Expect.err
        ]


fuzzUInt : Fuzz.Fuzzer Int
fuzzUInt =
    Fuzz.intRange 0 Random.maxInt
