module Urbit exposing
    ( Session, url, ship, uid, lastEventId
    , InMsg, InMsgData(..)
    , OutMsg
    , connect, connectUnauth, setupEventSource, messages, ack
    , poke, subscribe, unsubscribe, disconnect
    , send, sendBatch, sendTask, sendBatchTask
    , login, scry, spider
    , genChannelId, genUid
    )

{-|


# Data Types

@docs Session, url, ship, uid, lastEventId
@docs InMsg, InMsgData
@docs OutMsg


# Connection Setup

These are **required** in order to maintain an active connection to an urbit
ship and to perform poke/subscription requests.

@docs connect, connectUnauth, setupEventSource, messages, ack


# Stateful Requests

@docs poke, subscribe, unsubscribe, disconnect
@docs send, sendBatch, sendTask, sendBatchTask


# Stateless Requests

These do not require an active connection, however they do at least require you
to be authenticated.

@docs login, scry, spider


# Helpers

@docs genChannelId, genUid

-}

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import RadixInt
import Random
import Task exposing (Task)
import Time
import Urbit.Encoding.Atom exposing (Atom)
import Urbit.Encoding.Phonemic as Phonemic



-- DATA TYPES


{-| A session represents an active connection to an urbit ship.
-}
type Session
    = Session
        { url : String
        , ship : Atom
        , uid : String
        , lastEventId : Int
        }


{-| Represents an outgoing message.
-}
type OutMsg
    = OutMsg
        { action : String
        , pairs : List ( String, JE.Value )
        }


{-| Represents an incoming message.
-}
type alias InMsg =
    { lastEventId : Int
    , data : InMsgData
    }


{-| An incoming message may be one of the following:

  - `Poke` represents a response for a previous poke
  - `Subscribe` represents a response for a subscription requests
  - `Diff` represents a new incoming diff for a subscription
  - `Quit` means you have been kicked from a subscription and should likely
    attempt to resubscribe

In all cases the first `Int` value represents the Id of the message it is
relevant to.

-}
type InMsgData
    = Poke Int (Result String ())
    | Subscribe Int (Result String ())
    | Diff Int JE.Value
    | Quit Int


{-| Get a Session's url.
-}
url : Session -> String
url (Session session) =
    session.url


{-| Get a Session's ship.
-}
ship : Session -> Atom
ship (Session session) =
    session.ship


{-| Get a Session's uid.
-}
uid : Session -> String
uid (Session session) =
    session.uid


{-| Get a Session's lastEventId.
-}
lastEventId : Session -> Int
lastEventId (Session session) =
    session.lastEventId



-- CONNECTION SETUP


{-| Connect to an urbit ship and return a `Session` on success. Should be
followed by [setupEventSource](#setupEventSource) (this must be done as a
separate step due to the nature of Elm ports).

You can generate a unique `channelId` any way you wish, however the provided
[genChannelId](#genChannelId) function is recommended.

-}
connect :
    { ship : Atom
    , url : String
    , channelId : String
    , code : String
    }
    -> (Result Http.Error Session -> msg)
    -> Cmd msg
connect config tagger =
    loginTask
        { url = config.url
        , code = config.code
        }
        |> Task.andThen
            (\() ->
                let
                    session_ =
                        Session
                            { url = config.url
                            , ship = config.ship
                            , uid = config.channelId
                            , lastEventId = 0
                            }

                    ( session, task ) =
                        poke
                            { ship = config.ship
                            , app = "hood"
                            , mark = "helm-hi"
                            , json = JE.string "opening airlock"
                            }
                            |> sendTask session_
                in
                task |> Task.map (always session)
            )
        |> Task.attempt tagger


{-| Alternative to [connect](#connect) that does not require a `code`.

**Note:** Ensure the correct cookie is set through some other means beforehand
or else this will not work. Hosting from within an urbit using the :file-server
app with public set to false should work, for example.

-}
connectUnauth :
    { ship : Atom
    , url : String
    , channelId : String
    }
    -> (Result Http.Error Session -> msg)
    -> Cmd msg
connectUnauth config tagger =
    let
        ( session, cmd ) =
            let
                session_ =
                    Session
                        { url = config.url
                        , ship = config.ship
                        , uid = config.channelId
                        , lastEventId = 0
                        }
            in
            poke
                { ship = config.ship
                , app = "hood"
                , mark = "helm-hi"
                , json = JE.string "opening airlock"
                }
                |> send session_ identity
    in
    cmd
        |> Cmd.map (Result.map (\_ -> session) >> tagger)


{-| This will set up the realtime communication channel with urbit. Because Elm
does not support the [EventSource](https://developer.mozilla.org/en-US/docs/Web/API/EventSource)
API required, this must be done using ports. Elm does not allow ports to be
published as part of a package, but luckily the setup for this is fairly
straightforward.

First, copy/paste the following into your js file and replace any references to
`app` with the variable name of your running Elm app:

```js
app.ports.setupUrbitEventSource.subscribe((url) => {
  const eventSource = new EventSource(url, { withCredentials: true });

  eventSource.onmessage = function (event) {
    app.ports.onUrbitMessage.send({ message: event });
  };

  eventSource.onerror = function (event) {
    app.ports.onUrbitMessage.send({ error: event });
  };
});
```

Next, declare a port module and the following ports to handle the initial setup
and incoming messages:

    port setupUrbitEventSource : String -> Cmd msg

    port onUrbitMessage : (JE.Value -> msg) -> Sub msg

Now, the newly created `setupUrbitEventSource` can be passed as the first
argument to our [setupEventSource](#setupEventSource) function and used to
perform the necessary setup. Similarly, the `onUrbitMessage` function can be
passed as the first argument to [messages](#messages) below to set up the
subscription to incoming events.

    setupCommand =
        Urbit.setupEventSource setupUrbitEventSource session

    messagesSubscription =
        Urbit.messages onUrbitMessage GotUrbitMessage

See the Elm [guide](https://guide.elm-lang.org/interop/ports.html) for further
information regarding ports.

-}
setupEventSource : (String -> Cmd msg) -> Session -> Cmd msg
setupEventSource setupUrbitEventSource session_ =
    setupUrbitEventSource (channelUrl session_)


{-| Subscribe to incoming messages from urbit. See [setupEventSource](#setupEventSource)
above for instructions on how to use this. Every message received _must_ be
acknowledged by using the related [ack](#ack) function, otherwise urbit will
stop accepting any further messages.
-}
messages :
    ((JE.Value -> msg) -> Sub msg)
    -> (Result JD.Error InMsg -> msg)
    -> Sub msg
messages onUrbitMessage tagger =
    onUrbitMessage
        (JD.decodeValue incomingMessageDecoder
            >> Result.andThen identity
            >> tagger
        )


{-| Acknowledge an incoming message by its Id. Must be performed for every
incoming message received.
-}
ack : Int -> OutMsg
ack lastEventId_ =
    OutMsg
        { action = "ack"
        , pairs = [ ( "event-id", JE.int lastEventId_ ) ]
        }



-- STATEFUL REQUESTS


{-| Poke an app on a ship.
-}
poke :
    { ship : Atom
    , app : String
    , mark : String
    , json : JE.Value
    }
    -> OutMsg
poke config =
    appMsg
        { action = "poke"
        , ship = config.ship
        , app = config.app
        , pairs =
            [ ( "mark", JE.string config.mark )
            , ( "json", config.json )
            ]
        }


{-| Subscribe to ship events on some path.
-}
subscribe :
    { ship : Atom
    , app : String
    , path : String
    }
    -> OutMsg
subscribe config =
    appMsg
        { action = "subscribe"
        , ship = config.ship
        , app = config.app
        , pairs = [ ( "path", JE.string config.path ) ]
        }


{-| Unsubscribe from an existing subscription by its Id.
-}
unsubscribe : Int -> OutMsg
unsubscribe subscription =
    OutMsg
        { action = "unsubscribe"
        , pairs = [ ( "subscription", JE.int subscription ) ]
        }


{-| Safely deletes a channel.
-}
disconnect : OutMsg
disconnect =
    OutMsg
        { action = "delete"
        , pairs = []
        }


appMsg :
    { action : String
    , ship : Atom
    , app : String
    , pairs : List ( String, JE.Value )
    }
    -> OutMsg
appMsg config =
    OutMsg
        { action = config.action
        , pairs =
            ( "ship"
            , JE.string (config.ship |> Phonemic.toPatp |> String.dropLeft 1)
            )
                :: ( "app", JE.string config.app )
                :: config.pairs
        }


{-| Sends an outgoing message to urbit. Returns a tuple of a new session and
the command.
-}
send :
    Session
    -> (Result Http.Error () -> msg)
    -> OutMsg
    -> ( Session, Cmd msg )
send session tagger outMsg =
    outMsg
        |> List.singleton
        |> sendBatch session tagger


{-| Sends multiple outgoing messages to urbit in one request. Returns a tuple of
a new session and the command.
-}
sendBatch :
    Session
    -> (Result Http.Error () -> msg)
    -> List OutMsg
    -> ( Session, Cmd msg )
sendBatch session tagger outMsgs =
    sendBatchTask session outMsgs |> Tuple.mapSecond (Task.attempt tagger)


{-| Sends an outgoing message to urbit as a task. Returns a tuple of a new
session and the task.
-}
sendTask : Session -> OutMsg -> ( Session, Task Http.Error () )
sendTask session outMsg =
    outMsg
        |> List.singleton
        |> sendBatchTask session


{-| Sends multiple outgoing messages to urbit in one request as a task. Returns
a tuple of a new session and the task.
-}
sendBatchTask : Session -> List OutMsg -> ( Session, Task Http.Error () )
sendBatchTask session outMsgs =
    let
        (Session sessionData) =
            session
    in
    ( Session
        { sessionData
            | lastEventId = sessionData.lastEventId + List.length outMsgs
        }
    , Http.riskyTask
        { method = "PUT"
        , headers = []
        , url = channelUrl session
        , body =
            outMsgs
                |> List.indexedMap
                    (\index (OutMsg { action, pairs }) ->
                        JE.object
                            (( "id"
                             , JE.int <| sessionData.lastEventId + 1 + index
                             )
                                :: ( "action", JE.string action )
                                :: pairs
                            )
                    )
                |> JE.list identity
                |> Http.jsonBody
        , resolver =
            Http.bytesResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Err (Http.BadUrl url_)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ _ _ ->
                            Result.mapError Http.BadBody (Ok ())
                )
        , timeout = Nothing
        }
    )



-- STATELESS REQUESTS


{-| Login to an urbit without maintaining an active connection. This will allow
for stateless scry and spider requests that require authentication, but not
others.

**Note:** This is not required if you have already used [connect](#connect) to
authenticate.

-}
login :
    { url : String
    , code : String
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
login config tagger =
    loginTask config
        |> Task.attempt tagger


loginTask :
    { url : String
    , code : String
    }
    -> Task Http.Error ()
loginTask config =
    Http.riskyTask
        { method = "POST"
        , headers = []
        , url = config.url ++ "/~/login"
        , body =
            Http.stringBody
                "application/x-www-form-urlencoded"
                ("password=" ++ config.code)
        , resolver = Http.bytesResolver (\_ -> Ok ())
        , timeout = Nothing
        }


{-| Make a scry request.
-}
scry :
    { url : String
    , app : String
    , path : String
    , mark : String
    , decoder : JD.Decoder a
    }
    -> (Result Http.Error a -> msg)
    -> Cmd msg
scry config tagger =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url =
            config.url
                ++ "/~/scry/"
                ++ config.app
                ++ config.path
                ++ "."
                ++ config.mark
        , body = Http.emptyBody
        , expect = Http.expectJson tagger config.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Start a thread and get the response.
-}
spider :
    { url : String
    , inputMark : String
    , outputMark : String
    , threadName : String
    , body : JE.Value
    }
    -> (Result Http.Error JD.Value -> msg)
    -> Cmd msg
spider config tagger =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url =
            config.url
                ++ "/spider/"
                ++ config.inputMark
                ++ "/"
                ++ config.threadName
                ++ "/"
                ++ config.outputMark
                ++ ".json"
        , body = Http.jsonBody config.body
        , expect = Http.expectJson tagger JD.value
        , timeout = Nothing
        , tracker = Nothing
        }



-- INTERNAL HELPERS


channelUrl : Session -> String
channelUrl (Session session) =
    session.url ++ "/~/channel/" ++ session.uid



-- DECODERS


incomingMessageDecoder : Decoder (Result JD.Error InMsg)
incomingMessageDecoder =
    JD.field "message" messageDecoder
        |> JD.map Ok


messageDecoder : Decoder InMsg
messageDecoder =
    JD.map2 InMsg
        (JD.field "lastEventId" lastEventIdDecoder)
        (JD.field "data" dataDecoder)


lastEventIdDecoder : Decoder Int
lastEventIdDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case String.toInt str of
                    Just int ->
                        JD.succeed int

                    Nothing ->
                        JD.fail ("Invalid lastEventId: " ++ str)
            )


dataDecoder : Decoder InMsgData
dataDecoder =
    let
        responseDecoder =
            JD.oneOf
                [ JD.field "ok" <| JD.succeed (Ok ())
                , JD.field "err" JD.string |> JD.map Err
                ]

        decoder =
            JD.field "response" JD.string
                |> JD.andThen
                    (\response ->
                        case response of
                            "poke" ->
                                JD.map2 Poke
                                    (JD.field "id" JD.int)
                                    responseDecoder

                            "subscribe" ->
                                JD.map2 Subscribe
                                    (JD.field "id" JD.int)
                                    responseDecoder

                            "diff" ->
                                JD.map2 Diff
                                    (JD.field "id" JD.int)
                                    (JD.field "json" JD.value)

                            "quit" ->
                                JD.map Quit
                                    (JD.field "id" JD.int)

                            _ ->
                                JD.fail "Unrecognized response"
                    )
    in
    JD.string
        |> JD.andThen
            (\str ->
                case JD.decodeString decoder str of
                    Ok data ->
                        JD.succeed data

                    Err _ ->
                        JD.fail "Invalid data"
            )



-- HELPERS


{-| Recommended way to create a unique id for a session's channel. Takes the
current time and returns a random generator which can then be used to generate
the full ID.
-}
genChannelId : Time.Posix -> Random.Generator String
genChannelId time =
    let
        timestamp =
            String.fromInt (Time.posixToMillis time)

        randHexChar =
            Random.uniform
                '0'
                [ '1'
                , '2'
                , '3'
                , '4'
                , '5'
                , '6'
                , '7'
                , '8'
                , '9'
                , 'a'
                , 'b'
                , 'c'
                , 'd'
                , 'e'
                , 'f'
                ]
    in
    randomString 6 randHexChar
        |> Random.map (\randHex -> timestamp ++ "-" ++ randHex)


randomString : Int -> Random.Generator Char -> Random.Generator String
randomString stringLength charGenerator =
    Random.map String.fromList (Random.list stringLength charGenerator)


{-| Helper for generating random urbit UIDs.

Based off of <https://github.com/urbit/urbit/blob/137e4428f617c13f28ed31e520eff98d251ed3e9/pkg/interface/src/lib/util.js#L3>

-}
genUid : Random.Generator String
genUid =
    Random.map2
        (\head tail -> "0v" ++ String.fromInt head ++ "." ++ String.join "." tail)
        (Random.int 1 8)
        (Random.list 5 randomBase32Chunk)


randomBase32Chunk : Random.Generator String
randomBase32Chunk =
    Random.int 1 10000000
        |> Random.map
            (\d ->
                RadixInt.fromInt (RadixInt.Base 32) d
                    |> RadixInt.toList
                    |> List.map base32IntToChar
                    |> List.reverse
                    |> String.fromList
                    |> String.padLeft 5 '0'
                    |> String.right 5
            )


base32IntToChar : Int -> Char
base32IntToChar int =
    case int of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        15 ->
            'f'

        16 ->
            'g'

        17 ->
            'h'

        18 ->
            'i'

        19 ->
            'j'

        20 ->
            'k'

        21 ->
            'l'

        22 ->
            'm'

        23 ->
            'n'

        24 ->
            'o'

        25 ->
            'p'

        26 ->
            'q'

        27 ->
            'r'

        28 ->
            's'

        29 ->
            't'

        30 ->
            'u'

        31 ->
            'v'

        _ ->
            '0'
