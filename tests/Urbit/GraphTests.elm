module Urbit.GraphTests exposing (..)

import Expect
import Test exposing (Test, test)
import Urbit.Encoding.Atom as Atom
import Urbit.Graph as Graph


suite : Test
suite =
    test "parseResource" <|
        \() ->
            let
                parsed =
                    Maybe.map
                        (\ship ->
                            { ship = ship
                            , name = "hello"
                            }
                        )
                        (Atom.fromInt 0)
            in
            Graph.parseResource "~zod/hello"
                |> Result.toMaybe
                |> Expect.equal parsed
