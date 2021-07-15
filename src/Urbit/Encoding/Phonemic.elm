module Urbit.Encoding.Phonemic exposing
    ( toPatp, fromPatp, patpParser
    , toPatq, fromPatq, patqParser
    )

{-| Encoding/Decoding module for urbit @p (scrambled) and @q (unscrambled)
representations of numbers, in accordance with <https://urbit.org/docs/hoon/reference/auras>.


# @p

Phonemic Base (Ship Names)

@docs toPatp, fromPatp, patpParser


# @q

Phonemic Base, Unscrambled

@docs toPatq, fromPatq, patqParser

-}

import BigInt exposing (BigInt)
import Bitwise
import Murmur3
import Parser as P exposing ((|.), (|=), DeadEnd, Parser)
import Urbit.Encoding.Atom as Atom exposing (Atom)
import Urbit.Encoding.Shared exposing (zero)
import Word.Hex



-- @p


{-| Try to convert a BigInt to a @p encoded string, failing on negative numbers.

    import Urbit.Encoding.Atom as Atom

    Atom.fromInt 65536 |> Maybe.map toPatp
    --> Just "~dapnep-ronmyl"

-}
toPatp : Atom -> String
toPatp =
    Atom.toBigInt >> scramble >> encoder Patp


{-| Try to convert a @p encoded string into a BigInt, failing on improperly
formatted strings.

    import Urbit.Encoding.Atom as Atom
    import BigInt

    fromPatp "~sampel-palnet"
        |> Result.map (Atom.toBigInt >> BigInt.toString)
    --> Ok "1624961343"

-}
fromPatp : String -> Result (List DeadEnd) Atom
fromPatp patp =
    P.run patpParser patp


{-| Parser for @p encoded strings.
-}
patpParser : Parser Atom
patpParser =
    let
        parseExactlyFourPairs : List Int -> Parser (P.Step state (List Int))
        parseExactlyFourPairs digits =
            P.succeed
                (\( pre1, suf1 ) ( pre2, suf2 ) ( pre3, suf3 ) ( pre4, suf4 ) ->
                    P.Done
                        (suf4
                            :: pre4
                            :: suf3
                            :: pre3
                            :: suf2
                            :: pre2
                            :: suf1
                            :: pre1
                            :: digits
                        )
                )
                |. P.symbol "-"
                |= dashPairParser
                |= dashPairParser
                |= dashPairParser
                |= dashPairParser
    in
    headParser
        |> P.andThen (dashPairsParser (Just 4))
        |> P.andThen
            (\head ->
                P.loop head
                    (\digits ->
                        P.oneOf
                            [ parseExactlyFourPairs digits
                            , P.succeed (\() -> P.Done digits)
                                |= P.end
                            ]
                    )
            )
        |> P.andThen (radixToBigInt >> unscramble >> bigIntToAtomParser)



-- @q


{-| Try to convert a BigInt to a @q encoded string, failing on negative numbers.

    import Urbit.Encoding.Atom as Atom

    Atom.fromInt 5 |> Maybe.map toPatq
    --> Just "~per"

-}
toPatq : Atom -> String
toPatq =
    Atom.toBigInt >> encoder Patq


{-| Try to convert a @q encoded string into a BigInt, failing on improperly
formatted strings.

    import Urbit.Encoding.Atom as Atom
    import BigInt

    fromPatq "~sampel"
        |> Result.map (Atom.toBigInt >> BigInt.toString)
    --> Ok "1135"

-}
fromPatq : String -> Result (List DeadEnd) Atom
fromPatq patq =
    P.run patqParser patq


{-| Parser for @q encoded strings.
-}
patqParser : Parser Atom
patqParser =
    headParser
        |> P.andThen (dashPairsParser Nothing)
        |> P.andThen (radixToBigInt >> bigIntToAtomParser)



-- ENCODER


type Kind
    = Patp
    | Patq


encoder : Kind -> BigInt -> String
encoder kind point =
    let
        toString : List Int -> String
        toString digits =
            List.foldl
                (\digit ( index, acc ) ->
                    let
                        isEven =
                            modBy 2 index == 0

                        syllable =
                            if isEven then
                                intToSyllable suffixes digit

                            else
                                intToSyllable prefixes digit

                        patqSep =
                            if isEven then
                                "-"

                            else
                                ""

                        sep =
                            case ( index, kind ) of
                                ( 0, _ ) ->
                                    ""

                                ( _, Patp ) ->
                                    if modBy 8 index == 0 then
                                        "--"

                                    else
                                        patqSep

                                ( _, Patq ) ->
                                    patqSep
                    in
                    ( index + 1, syllable ++ sep ++ acc )
                )
                ( 0, "" )
                digits
                |> Tuple.second
                |> (++) "~"
    in
    point
        |> bigIntToRadix
        |> toString



-- RADIX


bigIntToRadix : BigInt -> List Int
bigIntToRadix int =
    let
        convertBigIntToRadix : BigInt -> List Int -> List Int
        convertBigIntToRadix int_ radix =
            let
                abs =
                    BigInt.abs int_

                remainder =
                    BigInt.modBy u_256 abs
                        |> Maybe.andThen (BigInt.toString >> String.toInt)
                        |> Maybe.withDefault 0

                quotient =
                    BigInt.div abs u_256
            in
            case BigInt.compare int_ zero of
                EQ ->
                    List.reverse radix

                _ ->
                    convertBigIntToRadix quotient (remainder :: radix)

        emptyRadixToZero : List Int -> List Int
        emptyRadixToZero radix =
            case radix of
                [] ->
                    [ 0 ]

                _ ->
                    radix
    in
    convertBigIntToRadix int []
        |> emptyRadixToZero


radixToBigInt : List Int -> BigInt
radixToBigInt =
    List.indexedMap Tuple.pair
        >> List.map
            (\( index, digit ) ->
                BigInt.pow u_256 (BigInt.fromInt index)
                    |> BigInt.mul (BigInt.fromInt digit)
            )
        >> List.foldl BigInt.add zero



-- PARSERS


bigIntToAtomParser : BigInt -> Parser Atom
bigIntToAtomParser bigInt =
    case Atom.fromBigInt bigInt of
        Just atom ->
            P.succeed atom

        Nothing ->
            P.problem "Invalid (negative) number"


{-| Parses a sequence of `dash -> prefix -> suffix`.
-}
dashPairsParser : Maybe Int -> List Int -> Parser (List Int)
dashPairsParser maybeLimit head =
    P.loop ( 1, head )
        (\( loopCount, digits ) ->
            let
                dashPairLoopParser =
                    P.succeed
                        (\( pre, suf ) ->
                            P.Loop
                                ( loopCount + 1
                                , suf :: pre :: digits
                                )
                        )
                        |= dashPairParser
            in
            P.oneOf
                [ case maybeLimit of
                    Just loopLimit ->
                        if loopCount == loopLimit then
                            P.succeed (P.Done digits)

                        else
                            dashPairLoopParser

                    Nothing ->
                        dashPairLoopParser
                , P.succeed (\() -> P.Done digits)
                    |= P.end
                ]
        )


{-| Parses the head of a phoneme, such as "~zod" or "~dozzod", which, unlike the
rest of the phoneme, is allowed to contain only a single suffix with no prefix.
However, it will still fail if there is only a prefix such as "~doz".

**Note:** The resulting list is in reverse order for efficiency.

-}
headParser : Parser (List Int)
headParser =
    P.succeed identity
        |. P.symbol "~"
        |= P.oneOf
            [ P.succeed (\suf -> [ suf ])
                |= suffixParser
            , P.succeed (\( pre, suf ) -> [ suf, pre ])
                |= pairParser
            ]


{-| Parses a valid pair of prefix and suffix such as "dozzod".

**Note:** Unlike headParser the resulting pair is not in reverse order.

-}
pairParser : Parser ( Int, Int )
pairParser =
    P.succeed Tuple.pair
        |= prefixParser
        |= suffixParser


{-| Same as pairParser except also requires a leading dash such as "-dozzod".
-}
dashPairParser : Parser ( Int, Int )
dashPairParser =
    P.succeed identity
        |. P.symbol "-"
        |= pairParser


suffixParser : Parser Int
suffixParser =
    syllablesToParser suffixes


prefixParser : Parser Int
prefixParser =
    syllablesToParser prefixes


syllablesToParser : List String -> Parser Int
syllablesToParser syllables =
    syllables
        |> List.indexedMap
            (\int syl ->
                P.succeed int
                    |. P.token syl
            )
        |> P.oneOf


intToSyllable : List String -> Int -> String
intToSyllable list index =
    list
        |> List.drop index
        |> List.head
        |> Maybe.withDefault ""


prefixes : List String
prefixes =
    [ "doz", "mar", "bin", "wan", "sam", "lit", "sig", "hid", "fid", "lis", "sog", "dir", "wac", "sab", "wis", "sib", "rig", "sol", "dop", "mod", "fog", "lid", "hop", "dar", "dor", "lor", "hod", "fol", "rin", "tog", "sil", "mir", "hol", "pas", "lac", "rov", "liv", "dal", "sat", "lib", "tab", "han", "tic", "pid", "tor", "bol", "fos", "dot", "los", "dil", "for", "pil", "ram", "tir", "win", "tad", "bic", "dif", "roc", "wid", "bis", "das", "mid", "lop", "ril", "nar", "dap", "mol", "san", "loc", "nov", "sit", "nid", "tip", "sic", "rop", "wit", "nat", "pan", "min", "rit", "pod", "mot", "tam", "tol", "sav", "pos", "nap", "nop", "som", "fin", "fon", "ban", "mor", "wor", "sip", "ron", "nor", "bot", "wic", "soc", "wat", "dol", "mag", "pic", "dav", "bid", "bal", "tim", "tas", "mal", "lig", "siv", "tag", "pad", "sal", "div", "dac", "tan", "sid", "fab", "tar", "mon", "ran", "nis", "wol", "mis", "pal", "las", "dis", "map", "rab", "tob", "rol", "lat", "lon", "nod", "nav", "fig", "nom", "nib", "pag", "sop", "ral", "bil", "had", "doc", "rid", "moc", "pac", "rav", "rip", "fal", "tod", "til", "tin", "hap", "mic", "fan", "pat", "tac", "lab", "mog", "sim", "son", "pin", "lom", "ric", "tap", "fir", "has", "bos", "bat", "poc", "hac", "tid", "hav", "sap", "lin", "dib", "hos", "dab", "bit", "bar", "rac", "par", "lod", "dos", "bor", "toc", "hil", "mac", "tom", "dig", "fil", "fas", "mit", "hob", "har", "mig", "hin", "rad", "mas", "hal", "rag", "lag", "fad", "top", "mop", "hab", "nil", "nos", "mil", "fop", "fam", "dat", "nol", "din", "hat", "nac", "ris", "fot", "rib", "hoc", "nim", "lar", "fit", "wal", "rap", "sar", "nal", "mos", "lan", "don", "dan", "lad", "dov", "riv", "bac", "pol", "lap", "tal", "pit", "nam", "bon", "ros", "ton", "fod", "pon", "sov", "noc", "sor", "lav", "mat", "mip", "fip" ]


suffixes : List String
suffixes =
    [ "zod", "nec", "bud", "wes", "sev", "per", "sut", "let", "ful", "pen", "syt", "dur", "wep", "ser", "wyl", "sun", "ryp", "syx", "dyr", "nup", "heb", "peg", "lup", "dep", "dys", "put", "lug", "hec", "ryt", "tyv", "syd", "nex", "lun", "mep", "lut", "sep", "pes", "del", "sul", "ped", "tem", "led", "tul", "met", "wen", "byn", "hex", "feb", "pyl", "dul", "het", "mev", "rut", "tyl", "wyd", "tep", "bes", "dex", "sef", "wyc", "bur", "der", "nep", "pur", "rys", "reb", "den", "nut", "sub", "pet", "rul", "syn", "reg", "tyd", "sup", "sem", "wyn", "rec", "meg", "net", "sec", "mul", "nym", "tev", "web", "sum", "mut", "nyx", "rex", "teb", "fus", "hep", "ben", "mus", "wyx", "sym", "sel", "ruc", "dec", "wex", "syr", "wet", "dyl", "myn", "mes", "det", "bet", "bel", "tux", "tug", "myr", "pel", "syp", "ter", "meb", "set", "dut", "deg", "tex", "sur", "fel", "tud", "nux", "rux", "ren", "wyt", "nub", "med", "lyt", "dus", "neb", "rum", "tyn", "seg", "lyx", "pun", "res", "red", "fun", "rev", "ref", "mec", "ted", "rus", "bex", "leb", "dux", "ryn", "num", "pyx", "ryg", "ryx", "fep", "tyr", "tus", "tyc", "leg", "nem", "fer", "mer", "ten", "lus", "nus", "syl", "tec", "mex", "pub", "rym", "tuc", "fyl", "lep", "deb", "ber", "mug", "hut", "tun", "byl", "sud", "pem", "dev", "lur", "def", "bus", "bep", "run", "mel", "pex", "dyt", "byt", "typ", "lev", "myl", "wed", "duc", "fur", "fex", "nul", "luc", "len", "ner", "lex", "rup", "ned", "lec", "ryd", "lyd", "fen", "wel", "nyd", "hus", "rel", "rud", "nes", "hes", "fet", "des", "ret", "dun", "ler", "nyr", "seb", "hul", "ryl", "lud", "rem", "lys", "fyn", "wer", "ryc", "sug", "nys", "nyl", "lyn", "dyn", "dem", "lux", "fed", "sed", "bec", "mun", "lyr", "tes", "mud", "nyt", "byr", "sen", "weg", "fyr", "mur", "tel", "rep", "teg", "pec", "nel", "nev", "fes" ]



-- Shared Scramble/Unscramble


scramble : BigInt -> BigInt
scramble arg =
    let
        loop pyn =
            let
                lo =
                    bigIntAnd pyn ux_ffff_ffff

                hi =
                    bigIntAnd pyn ux_ffff_ffff_0000_0000
            in
            if BigInt.gte pyn ux_1_0000 && BigInt.lte pyn ux_ffff_ffff then
                BigInt.add ux_1_0000 (feis (BigInt.sub pyn ux_1_0000))

            else if
                BigInt.gte pyn ux_1_0000_0000
                    && BigInt.lte pyn ux_ffff_ffff_ffff_ffff
            then
                bigIntOr hi (loop lo)

            else
                pyn
    in
    loop arg


unscramble : BigInt -> BigInt
unscramble arg =
    let
        loop cry =
            let
                lo =
                    bigIntAnd cry ux_ffff_ffff

                hi =
                    bigIntAnd cry ux_ffff_ffff_0000_0000
            in
            if BigInt.gte cry ux_1_0000 && BigInt.lte cry ux_ffff_ffff then
                BigInt.add ux_1_0000 (tail (BigInt.sub cry ux_1_0000))

            else if
                BigInt.gte cry ux_1_0000_0000
                    && BigInt.lte cry ux_ffff_ffff_ffff_ffff
            then
                bigIntOr hi (loop lo)

            else
                cry
    in
    loop arg



-- Shared Scramble/Unscramble Helpers


muk : Int -> BigInt -> Int
muk syd key =
    let
        lo =
            bigIntAnd key ux_ff |> bigIntToIntUnsafe

        hi =
            BigInt.div (bigIntAnd key ux_ff00) u_256 |> bigIntToIntUnsafe

        kee =
            String.fromList
                [ Char.fromCode lo
                , Char.fromCode hi
                ]
    in
    Murmur3.hashString syd kee


prf : Int -> BigInt -> BigInt
prf j arg =
    let
        raku =
            case j of
                0 ->
                    0xB76D5EED

                1 ->
                    0xEE281300

                2 ->
                    0x85BCAE01

                _ ->
                    0x4B387AF7
    in
    muk raku arg |> BigInt.fromInt



-- Scramble Helpers


feis : BigInt -> BigInt
feis arg =
    feistelCipher 4 u_65535 u_65536 ux_ffff_ffff prf arg


feistelCipher :
    Int
    -> BigInt
    -> BigInt
    -> BigInt
    -> (Int -> BigInt -> BigInt)
    -> BigInt
    -> BigInt
feistelCipher r a b k f m =
    let
        c =
            fe r a b f m
    in
    if BigInt.lt c k then
        c

    else
        fe r a b f c


fe : Int -> BigInt -> BigInt -> (Int -> BigInt -> BigInt) -> BigInt -> BigInt
fe r a b f m =
    let
        loop j ell arr =
            if j > r then
                if modBy 2 r /= 0 then
                    BigInt.mul a arr |> BigInt.add ell

                else
                    case BigInt.compare arr a of
                        EQ ->
                            BigInt.mul a arr |> BigInt.add ell

                        _ ->
                            BigInt.mul a ell |> BigInt.add arr

            else
                let
                    eff =
                        f (j - 1) arr

                    tmp =
                        if modBy 2 j /= 0 then
                            BigInt.add ell eff |> bigIntModByUnsafe a

                        else
                            BigInt.add ell eff |> bigIntModByUnsafe b
                in
                loop (j + 1) arr tmp

        ell_ =
            bigIntModByUnsafe a m

        arr_ =
            BigInt.div m a
    in
    loop 1 ell_ arr_



-- Unscramble Helpers


tail : BigInt -> BigInt
tail arg =
    feistelCipherRev 4 u_65535 u_65536 ux_ffff_ffff prf arg


feistelCipherRev :
    Int
    -> BigInt
    -> BigInt
    -> BigInt
    -> (Int -> BigInt -> BigInt)
    -> BigInt
    -> BigInt
feistelCipherRev r a b k f m =
    let
        c =
            fen r a b f m
    in
    if BigInt.lt c k then
        c

    else
        fen r a b f c


fen : Int -> BigInt -> BigInt -> (Int -> BigInt -> BigInt) -> BigInt -> BigInt
fen r a b f m =
    let
        loop j ell arr =
            if j < 1 then
                BigInt.mul a arr |> BigInt.add ell

            else
                let
                    eff =
                        f (j - 1) ell

                    tmp =
                        if modBy 2 j /= 0 then
                            BigInt.add arr a
                                |> BigInt.sub (bigIntModByUnsafe a eff)
                                |> BigInt.abs
                                |> bigIntModByUnsafe a

                        else
                            BigInt.add arr b
                                |> BigInt.sub (bigIntModByUnsafe b eff)
                                |> BigInt.abs
                                |> bigIntModByUnsafe b
                in
                loop (j - 1) tmp ell

        ahh =
            if modBy 2 r /= 0 then
                BigInt.div m a

            else
                bigIntModByUnsafe a m

        ale =
            if modBy 2 r /= 0 then
                bigIntModByUnsafe a m

            else
                BigInt.div m a

        ell_ =
            case BigInt.compare ale a of
                EQ ->
                    ahh

                _ ->
                    ale

        arr_ =
            case BigInt.compare ale a of
                EQ ->
                    ale

                _ ->
                    ahh
    in
    loop r ell_ arr_



-- CONSTANTS


u_256 : BigInt
u_256 =
    BigInt.fromInt 256


ux_ff : BigInt
ux_ff =
    BigInt.fromInt 0xFF


ux_ff00 : BigInt
ux_ff00 =
    BigInt.fromInt 0xFF00


ux_1_0000 : BigInt
ux_1_0000 =
    BigInt.fromInt 0x00010000


ux_ffff_ffff : BigInt
ux_ffff_ffff =
    bigIntFromHexUnsafe "ffffffff"


ux_1_0000_0000 : BigInt
ux_1_0000_0000 =
    bigIntFromHexUnsafe "100000000"


ux_ffff_ffff_ffff_ffff : BigInt
ux_ffff_ffff_ffff_ffff =
    bigIntFromHexUnsafe "ffffffffffffffff"


ux_ffff_ffff_0000_0000 : BigInt
ux_ffff_ffff_0000_0000 =
    bigIntFromHexUnsafe "ffffffff00000000"


u_65535 : BigInt
u_65535 =
    BigInt.fromInt 65535


u_65536 : BigInt
u_65536 =
    BigInt.fromInt 65536



-- BigInt Helpers


bigIntToIntUnsafe : BigInt -> Int
bigIntToIntUnsafe =
    BigInt.toString >> String.toInt >> Maybe.withDefault 0


bigIntModByUnsafe : BigInt -> BigInt -> BigInt
bigIntModByUnsafe modulus x =
    BigInt.modBy modulus x |> Maybe.withDefault zero


bigIntFromHexUnsafe : String -> BigInt
bigIntFromHexUnsafe hex =
    hex
        |> BigInt.fromHexString
        |> Maybe.withDefault zero


bigIntMap2Bytes : (Int -> Int -> Int) -> BigInt -> BigInt -> BigInt
bigIntMap2Bytes f a b =
    let
        toEvenLengthHexString str =
            if modBy 2 (String.length str) == 0 then
                str

            else
                "0" ++ str

        toRevByteList bigInt =
            bigInt
                |> BigInt.toHexString
                |> toEvenLengthHexString
                |> Word.Hex.toByteList
                |> List.reverse
    in
    List.map2 f
        (toRevByteList a)
        (toRevByteList b)
        |> List.reverse
        |> Word.Hex.fromByteList
        |> BigInt.fromHexString
        |> Maybe.withDefault zero


bigIntAnd : BigInt -> BigInt -> BigInt
bigIntAnd =
    bigIntMap2Bytes Bitwise.and


bigIntOr : BigInt -> BigInt -> BigInt
bigIntOr =
    bigIntMap2Bytes Bitwise.or
