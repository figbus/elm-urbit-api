module Urbit.Encoding.DateAbsolute exposing
    ( DateRecord
    , fromDateRecord, toDateRecord
    , fromPosix, toPosix
    , parseDateRecord, parsePosix
    )

{-| Encoding/Decoding module for urbit @da representations of absolute dates, in
accordance with <https://urbit.org/docs/hoon/reference/auras>.

**Note:** All dates in this format are in the UTC time zone.


# Date Record

@docs DateRecord
@docs fromDateRecord, toDateRecord


# Posix Time

@docs fromPosix, toPosix


# Parsing

@docs parseDateRecord, parsePosix

-}

import Derberos.Date.Core
import Parser as P exposing ((|.), (|=), DeadEnd, Parser)
import Time exposing (Month(..), Posix, utc)
import Word.Hex



-- DATE RECORD


{-| An absolute date stored as a record.
-}
type alias DateRecord =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millis : Int
    }


{-| Convert a [DateRecord](#DateRecord) to a @da encoded string.

    fromDateRecord
        { year = 2021
        , month = 8
        , day = 3
        , hour = 6
        , minute = 39
        , second = 55
        , millis = 356
        }
    --> "~2021.8.3..6.39.55..05b2"

-}
fromDateRecord : DateRecord -> String
fromDateRecord { year, month, day, hour, minute, second, millis } =
    let
        date =
            String.fromInt year
                ++ "."
                ++ String.fromInt month
                ++ "."
                ++ String.fromInt day

        time =
            if hour == 0 && minute == 0 && second == 0 && millis == 0 then
                ""

            else
                ".."
                    ++ String.fromInt hour
                    ++ "."
                    ++ String.fromInt minute
                    ++ "."
                    ++ String.fromInt second

        millis_ =
            if millis == 0 then
                ""

            else
                ".." ++ millisToHex millis
    in
    "~" ++ date ++ time ++ millis_


{-| Convert a @da encoded string to a [DateRecord](#DateRecord), failing on
improperly formatted strings.

    toDateRecord "~2021.8.3..6.39.55..05b2"
    --> Ok
    -->    { year = 2021
    -->    , month = 8
    -->    , day = 3
    -->    , hour = 6
    -->    , minute = 39
    -->    , second = 55
    -->    , millis = 356
    -->    }

-}
toDateRecord : String -> Result (List DeadEnd) DateRecord
toDateRecord string =
    P.run parseDateRecord string



-- POSIX TIME


{-| Convert a posix time to a @da encoded string.

    import Time

    Time.millisToPosix 1627948800000 |> fromPosix
    --> "~2021.8.3"

-}
fromPosix : Posix -> String
fromPosix posix =
    fromDateRecord
        { year = Time.toYear utc posix
        , month = Time.toMonth utc posix |> monthToInt
        , day = Time.toDay utc posix
        , hour = Time.toHour utc posix
        , minute = Time.toMinute utc posix
        , second = Time.toSecond utc posix
        , millis = Time.toMillis utc posix
        }


{-| Try to convert a @da encoded string to a posix time, failing on improperly
formatted strings.

    import Time

    toPosix "~2021.8.3..8.25.00"
    --> Ok (Time.millisToPosix 1627979100000)

-}
toPosix : String -> Result (List DeadEnd) Posix
toPosix string =
    P.run parsePosix string



-- PARSING


{-| Parses a @da encoded string into a [DateRecord](#DateRecord).
-}
parseDateRecord : Parser DateRecord
parseDateRecord =
    let
        timeParser :
            Parser
                { hour : Int
                , minute : Int
                , second : Int
                , millis : Int
                }
        timeParser =
            P.succeed
                (\hour minute seconds millis ->
                    { hour = hour
                    , minute = minute
                    , second = seconds
                    , millis = millis
                    }
                )
                |. P.symbol ".."
                |= parseIntIgnoreDot
                |. P.symbol "."
                |= parseIntIgnoreDot
                |. P.symbol "."
                |= parseIntIgnoreDot
                |= P.oneOf
                    [ P.end |> P.map (\() -> 0)
                    , subSecondParser
                    ]

        subSecondParser : Parser Int
        subSecondParser =
            P.succeed identity
                |. P.symbol ".."
                |= (P.chompWhile (\_ -> True)
                        |> P.getChompedString
                        |> P.andThen (hexToMillis >> P.succeed)
                   )
    in
    P.succeed
        (\year month day time ->
            { year = year
            , month = month
            , day = day
            , hour = time.hour
            , minute = time.minute
            , second = time.second
            , millis = time.millis
            }
        )
        |. P.symbol "~"
        |= parseIntIgnoreDot
        |. P.symbol "."
        |= parseIntIgnoreDot
        |. P.symbol "."
        |= parseIntIgnoreDot
        |= P.oneOf
            [ P.end
                |> P.map
                    (\() ->
                        { hour = 0
                        , minute = 0
                        , second = 0
                        , millis = 0
                        }
                    )
            , timeParser
            ]


{-| Parses a @da encoded string into a posix time.
-}
parsePosix : Parser Posix
parsePosix =
    parseDateRecord
        |> P.map
            (\d ->
                Derberos.Date.Core.civilToPosix
                    { year = d.year
                    , month = d.month
                    , day = d.day
                    , hour = d.hour
                    , minute = d.minute
                    , second = d.second
                    , millis = d.millis
                    , zone = Time.utc
                    }
            )



-- HELPERS


parseIntIgnoreDot : Parser Int
parseIntIgnoreDot =
    P.chompUntilEndOr "."
        |> P.getChompedString
        |> P.andThen
            (\str ->
                case String.toInt str of
                    Just int ->
                        P.succeed int

                    Nothing ->
                        P.problem ("Invalid int: " ++ str)
            )


hexToMillis : String -> Int
hexToMillis hex =
    hex
        |> Word.Hex.toByteList
        |> List.reverse
        |> List.foldl
            (\chunk ( index, total ) ->
                ( index + 1
                , total + (chunk * (256 ^ index))
                )
            )
            ( 0, 0 )
        |> Tuple.second
        |> (\total -> toFloat total / 4.096)
        |> round


millisToHex : Int -> String
millisToHex millis =
    toFloat millis
        * 4.096
        |> round
        |> Word.Hex.fromInt 4


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
