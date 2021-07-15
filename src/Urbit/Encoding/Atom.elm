module Urbit.Encoding.Atom exposing (Atom, fromInt, fromBigInt, toBigInt)

{-|

@docs Atom, fromInt, fromBigInt, toBigInt

-}

import BigInt exposing (BigInt)
import Urbit.Encoding.Shared exposing (zero)


{-| The most basic Urbit data type. Represents any non-negative integer of any
size.
-}
type Atom
    = Atom BigInt


{-| Creates an atom from an `Int`, failing for negative numbers.
-}
fromInt : Int -> Maybe Atom
fromInt =
    BigInt.fromInt >> fromBigInt


{-| Creates an atom from a `BigInt`, failing for negative numbers.
-}
fromBigInt : BigInt -> Maybe Atom
fromBigInt bigInt =
    if BigInt.gte bigInt zero then
        Just <| Atom bigInt

    else
        Nothing


{-| Converts an atom into a `BigInt`.
-}
toBigInt : Atom -> BigInt
toBigInt (Atom bigInt) =
    bigInt
