port module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as JD
import Json.Encode as JE
import Random
import Return
import Task
import Time exposing (Posix)
import Urbit exposing (MessageData(..))
import Urbit.Graph as Graph


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { entropy : Int
    , configInput : ConfigInput
    , session : Maybe Urbit.Session
    , graphStore : Graph.Store
    , textInput : String
    }


type alias ConfigInput =
    { url : String
    , ourShip : String
    , code : String
    , resource : String
    }


init : Int -> ( Model, Cmd Msg )
init entropy =
    ( { entropy = entropy
      , configInput =
            { url = "http://localhost:8080"
            , ourShip = "zod"
            , code = "lidlut-tabwed-pillex-ridrup"
            , resource = "~zod/airlock-test"
            }
      , session = Nothing
      , graphStore = Graph.emptyStore
      , textInput = ""
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | GotConfigInput ConfigInput
    | ConnectButtonPressed
    | GotInitTime Time.Posix
    | GotConnection (Result Http.Error Urbit.Session)
    | GotUrbitMessage (Result JD.Error Urbit.Message)
    | GotScry (Result Http.Error Graph.Update)
    | TextInput String
    | FormSubmitted
    | GotTimeForPost Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotConfigInput configInput ->
            ( { model | configInput = configInput }, Cmd.none )

        ConnectButtonPressed ->
            ( model
            , Time.now |> Task.perform GotInitTime
            )

        GotInitTime now ->
            ( model
            , Urbit.connect
                { ship = model.configInput.ourShip
                , url = model.configInput.url
                , channelId =
                    Random.initialSeed model.entropy
                        |> Random.step (Urbit.genChannelId now)
                        |> Tuple.first
                , code = model.configInput.code
                }
                GotConnection
            )

        GotConnection (Ok session) ->
            Graph.subscribeToGraphUpdates
                { ship = model.configInput.ourShip
                , session = session
                }
                (\_ -> NoOp)
                |> Return.andThen
                    (\newSession ->
                        ( { model | session = Just newSession }
                        , Cmd.batch
                            [ Urbit.setupEventSource
                                setupUrbitEventSource
                                newSession
                            , Graph.getGraph
                                { url = model.configInput.url
                                , resource =
                                    parseResource model.configInput.resource
                                }
                                GotScry
                            ]
                        )
                    )

        GotConnection (Err _) ->
            ( model, Cmd.none )

        GotUrbitMessage (Ok urbitMsg) ->
            case model.session of
                Just session ->
                    Urbit.ack
                        { lastEventId = urbitMsg.lastEventId
                        , session = session
                        }
                        (\_ -> NoOp)
                        |> Return.andThen
                            (\newSession ->
                                handleUrbitMessage
                                    urbitMsg.data
                                    newSession
                                    { model | session = Just newSession }
                            )

                Nothing ->
                    ( model, Cmd.none )

        GotUrbitMessage (Err _) ->
            ( model, Cmd.none )

        GotScry (Ok graphUpdate) ->
            ( { model
                | graphStore = Graph.updateStore graphUpdate model.graphStore
              }
            , Cmd.none
            )

        GotScry (Err (BadStatus 404)) ->
            ( model
            , Graph.createUnmanagedGraph
                { url = model.configInput.url
                , resource = parseResource model.configInput.resource
                , title = "Airlock Test"
                , description = ""
                , graphModule = "chat"
                , mark = "graph-validator-chat"
                }
                (always NoOp)
            )

        GotScry (Err _) ->
            ( model, Cmd.none )

        TextInput textInput ->
            ( { model | textInput = textInput }, Cmd.none )

        FormSubmitted ->
            ( model
            , Task.perform GotTimeForPost Time.now
            )

        GotTimeForPost now ->
            if String.isEmpty model.textInput then
                ( model, Cmd.none )

            else
                ( { model | textInput = "" }
                , Graph.addNodesSpider
                    { url = model.configInput.url
                    , resource = parseResource model.configInput.resource
                    , nodes =
                        [ Graph.newNode
                            { index =
                                [ String.fromInt (Time.posixToMillis now)
                                ]
                            , author = "~" ++ model.configInput.ourShip
                            , timeSent = now
                            , hash = Nothing
                            , contents = [ Graph.textContent model.textInput ]
                            }
                        ]
                    }
                    (\_ -> NoOp)
                )


handleUrbitMessage :
    Urbit.MessageData
    -> Urbit.Session
    -> Model
    -> ( Model, Cmd Msg )
handleUrbitMessage data session model =
    case data of
        Poke _ _ ->
            ( model, Cmd.none )

        Subscribe _ _ ->
            ( model, Cmd.none )

        Diff _ diff ->
            ( { model
                | graphStore =
                    case JD.decodeValue Graph.updateDecoder diff of
                        Ok graphUpdate ->
                            Graph.updateStore graphUpdate model.graphStore

                        Err _ ->
                            model.graphStore
              }
            , Cmd.none
            )

        Quit _ ->
            Graph.subscribeToGraphUpdates
                { ship = model.configInput.ourShip
                , session = session
                }
                (\_ -> NoOp)
                |> Return.map
                    (\newSession2 -> { model | session = Just newSession2 })


subscriptions : model -> Sub Msg
subscriptions _ =
    Urbit.messages onUrbitMessage GotUrbitMessage


view : Model -> Document Msg
view model =
    { title = "Airlock Example"
    , body =
        let
            configInput =
                model.configInput

            configInputField label value updateFn =
                Html.label
                    [ style "display" "block"
                    , style "margin-bottom" "0.5em"
                    ]
                    [ div
                        [ style "display" "block"
                        , style "margin-bottom" "0.25em"
                        ]
                        [ text label ]
                    , input
                        [ type_ "text"
                        , Html.Attributes.value value
                        , onInput (updateFn >> GotConfigInput)
                        ]
                        []
                    ]

            latestDateNode =
                model.graphStore
                    |> Graph.getFromStore
                        (parseResource model.configInput.resource)
                    |> Maybe.withDefault Dict.empty
                    |> Dict.toList
                    |> List.sortBy
                        (\( _, node ) ->
                            node
                                |> Graph.getNodePost
                                |> .timeSent
                                |> Time.posixToMillis
                        )
                    |> List.reverse
                    |> List.head

            status =
                case latestDateNode of
                    Just ( _, node ) ->
                        node
                            |> Graph.getNodePost
                            |> .contents
                            |> List.head
                            |> Maybe.andThen
                                (JD.decodeValue (JD.field "text" JD.string)
                                    >> Result.toMaybe
                                )
                            |> Maybe.withDefault "Invalid status"

                    Nothing ->
                        "Type something!"
        in
        case model.session of
            Nothing ->
                [ configInputField "Url"
                    configInput.url
                    (\val -> { configInput | url = val })
                , configInputField "Ship"
                    configInput.ourShip
                    (\val -> { configInput | ourShip = val })
                , configInputField "Code"
                    configInput.code
                    (\val -> { configInput | code = val })
                , configInputField "Resource"
                    configInput.resource
                    (\val -> { configInput | resource = val })
                , button [ onClick ConnectButtonPressed ] [ text "Connect" ]
                ]

            Just _ ->
                [ div [ style "margin" "1rem 0" ] [ text status ]
                , Html.form [ onSubmit NoOp ]
                    [ input
                        [ type_ "text"
                        , value model.textInput
                        , onInput TextInput
                        ]
                        []
                    , button [ onClick FormSubmitted ] [ text "Submit" ]
                    ]
                ]
    }



-- HELPERS


parseResource : String -> Graph.Resource
parseResource resource =
    case String.split "/" resource of
        [ ship, name ] ->
            { ship = ship
            , name = name
            }

        _ ->
            { ship = "~zod"
            , name = "airlock-test"
            }



-- PORTS


port setupUrbitEventSource : String -> Cmd msg


port onUrbitMessage : (JE.Value -> msg) -> Sub msg
