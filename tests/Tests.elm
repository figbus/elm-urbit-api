module Tests exposing (..)

import BigInt
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import Urbit.Encoding.Phonemic as Phonemic


suite : Test
suite =
    describe "Encoding.Phonemic"
        [ describe "@q"
            [ fuzz fuzzUInt "Encode/decode equivalence" <|
                \int ->
                    int
                        |> BigInt.fromInt
                        |> Phonemic.toPatq
                        |> Maybe.andThen (Phonemic.fromPatq >> Result.toMaybe)
                        |> Maybe.andThen (BigInt.toString >> String.toInt)
                        |> Expect.equal (Just int)
            , test "Cannot encode negative ints" <|
                \() ->
                    -1
                        |> BigInt.fromInt
                        |> Phonemic.toPatq
                        |> Expect.equal Nothing
            , test "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet" <|
                \() ->
                    "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet"
                        |> Phonemic.fromPatq
                        |> Result.map BigInt.toString
                        |> Expect.equal
                            (Ok "5895839060854263750826948981758590799")
            , test "5895839060854263750826948981758590799" <|
                \() ->
                    "5895839060854263750826948981758590799"
                        |> BigInt.fromIntString
                        |> Maybe.andThen Phonemic.toPatq
                        |> Expect.equal
                            (Just "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet")
            , test "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
                (\() ->
                    "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
                        |> Phonemic.fromPatq
                        |> Result.map BigInt.toString
                        |> Expect.err
                )
            ]
        , describe "@p"
            [ fuzz fuzzUInt "Encode/decode equivalence" <|
                \int ->
                    int
                        |> BigInt.fromInt
                        |> Phonemic.toPatp
                        |> Maybe.andThen (Phonemic.fromPatp >> Result.toMaybe)
                        |> Maybe.andThen (BigInt.toString >> String.toInt)
                        |> Expect.equal (Just int)
            , test "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
                (\() ->
                    "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet"
                        |> Phonemic.fromPatp
                        |> Result.map BigInt.toString
                        |> Expect.equal
                            (Ok "5895839060854263750826948981758590799")
                )
            , test "5895839060854263750826948981758590799" <|
                \() ->
                    "5895839060854263750826948981758590799"
                        |> BigInt.fromIntString
                        |> Maybe.andThen Phonemic.toPatp
                        |> Expect.equal
                            (Just "~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet")
            , test "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet" <|
                \() ->
                    "~sampel-palnet-sampel-palnet-sampel-palnet-sampel-palnet"
                        |> Phonemic.fromPatp
                        |> Result.map BigInt.toString
                        |> Expect.err
            ]
        ]


maxSafeInt : Int
maxSafeInt =
    2147483647


fuzzUInt : Fuzz.Fuzzer Int
fuzzUInt =
    Fuzz.intRange 0 maxSafeInt
