module Urbit.Encoding.DateAbsoluteTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (Test, describe, fuzz, test)
import Urbit.Encoding.DateAbsolute as DateAbsolute exposing (DateRecord)


toStringTests : Test
toStringTests =
    describe "toString"
        [ test "without milliseconds" <|
            \() ->
                { year = 2021
                , month = 8
                , day = 3
                , hour = 6
                , minute = 39
                , second = 55
                , millis = 0
                }
                    |> DateAbsolute.fromDateRecord
                    |> Expect.equal "~2021.8.3..6.39.55"
        , test "without time" <|
            \() ->
                { year = 2021
                , month = 8
                , day = 3
                , hour = 0
                , minute = 0
                , second = 0
                , millis = 0
                }
                    |> DateAbsolute.fromDateRecord
                    |> Expect.equal "~2021.8.3"
        ]


equivalenceTest : Test
equivalenceTest =
    fuzz dateFuzzer "equivalence" <|
        \date ->
            date
                |> DateAbsolute.fromDateRecord
                |> DateAbsolute.toDateRecord
                |> Expect.equal (Ok date)


dateFuzzer : Fuzzer DateRecord
dateFuzzer =
    Fuzz.map DateRecord
        (Fuzz.intRange 1 Random.maxInt)
        |> Fuzz.andMap (Fuzz.intRange 1 12)
        |> Fuzz.andMap (Fuzz.intRange 1 29)
        |> Fuzz.andMap (Fuzz.intRange 1 24)
        |> Fuzz.andMap (Fuzz.intRange 0 59)
        |> Fuzz.andMap (Fuzz.intRange 0 59)
        |> Fuzz.andMap (Fuzz.intRange 0 999)
