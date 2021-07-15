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
import Urbit exposing (InMsgData(..))
import Urbit.Encoding.Phonemic as Phonemic
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
    , connectionInfo : Maybe ConnectionInfo
    , graphStore : Graph.Store
    , textInput : String
    }


type alias ConnectionInfo =
    { resource : Graph.Resource
    , session : Urbit.Session
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
            , ourShip = "~zod"
            , code = "lidlut-tabwed-pillex-ridrup"
            , resource = "~zod/airlock-test"
            }
      , connectionInfo = Nothing
      , graphStore = Graph.emptyStore
      , textInput = ""
      }
    , Cmd.none
    )


type Msg
    = Ignore
    | GotConfigInput ConfigInput
    | ConnectButtonPressed
    | GotInitTime Time.Posix
    | GotConnection Graph.Resource (Result Http.Error Urbit.Session)
    | GotUrbitMessage (Result JD.Error Urbit.InMsg)
    | GotScry (Result Http.Error Graph.Update)
    | TextInput String
    | FormSubmitted
    | GotTimeForPost Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none )

        GotConfigInput configInput ->
            ( { model | configInput = configInput }, Cmd.none )

        ConnectButtonPressed ->
            ( model
            , Time.now |> Task.perform GotInitTime
            )

        GotInitTime now ->
            case
                ( Phonemic.fromPatp model.configInput.ourShip
                , Graph.parseResource model.configInput.resource
                )
            of
                ( Ok ship, Ok resource ) ->
                    ( model
                    , Urbit.connect
                        { ship = ship
                        , url = model.configInput.url
                        , channelId =
                            Random.initialSeed model.entropy
                                |> Random.step (Urbit.genChannelId now)
                                |> Tuple.first
                        , code = model.configInput.code
                        }
                        (GotConnection resource)
                    )

                _ ->
                    ( model, Cmd.none )

        GotConnection resource (Ok session) ->
            Graph.subscribeToGraphUpdates (Urbit.ship session)
                |> Urbit.send session (\_ -> Ignore)
                |> Return.andThen
                    (\newSession ->
                        ( { model
                            | connectionInfo =
                                Just
                                    { session = newSession
                                    , resource = resource
                                    }
                          }
                        , Cmd.batch
                            [ Urbit.setupEventSource
                                setupUrbitEventSource
                                newSession
                            , Graph.getGraph
                                { url = model.configInput.url
                                , resource = resource
                                }
                                GotScry
                            ]
                        )
                    )

        GotConnection _ (Err _) ->
            ( model, Cmd.none )

        GotUrbitMessage (Ok urbitMsg) ->
            case model.connectionInfo of
                Just connectionInfo ->
                    let
                        ( newModel, outMsgs ) =
                            handleUrbitMessage urbitMsg.data
                                connectionInfo.session
                                model
                    in
                    Urbit.ack urbitMsg.lastEventId
                        :: outMsgs
                        |> Urbit.sendBatch connectionInfo.session (\_ -> Ignore)
                        |> Return.map
                            (\newSession ->
                                { newModel
                                    | connectionInfo =
                                        Just
                                            { connectionInfo
                                                | session = newSession
                                            }
                                }
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
            case model.connectionInfo of
                Just { resource } ->
                    ( model
                    , Graph.createUnmanagedGraph
                        { url = model.configInput.url
                        , resource = resource
                        , title = "Airlock Test"
                        , description = ""
                        , invites = []
                        , graphModule = "chat"
                        , mark = "graph-validator-chat"
                        }
                        (always Ignore)
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotScry (Err _) ->
            ( model, Cmd.none )

        TextInput textInput ->
            ( { model | textInput = textInput }, Cmd.none )

        FormSubmitted ->
            ( model
            , Task.perform GotTimeForPost Time.now
            )

        GotTimeForPost now ->
            case ( model.connectionInfo, String.isEmpty model.textInput ) of
                ( Just { session, resource }, False ) ->
                    ( { model | textInput = "" }
                    , Graph.addNodesSpider
                        { url = model.configInput.url
                        , resource = resource
                        , nodes =
                            [ Graph.newNode
                                { index =
                                    [ String.fromInt (Time.posixToMillis now)
                                    ]
                                , author = Urbit.ship session
                                , timeSent = now
                                , hash = Nothing
                                , contents = [ Graph.textContent model.textInput ]
                                }
                            ]
                        }
                        (\_ -> Ignore)
                    )

                _ ->
                    ( model, Cmd.none )


handleUrbitMessage :
    Urbit.InMsgData
    -> Urbit.Session
    -> Model
    -> ( Model, List Urbit.OutMsg )
handleUrbitMessage data session model =
    case data of
        Poke _ _ ->
            ( model, [] )

        Subscribe _ _ ->
            ( model, [] )

        Diff _ diff ->
            ( { model
                | graphStore =
                    case JD.decodeValue Graph.updateDecoder diff of
                        Ok graphUpdate ->
                            Graph.updateStore graphUpdate model.graphStore

                        Err _ ->
                            model.graphStore
              }
            , []
            )

        Quit _ ->
            ( model
            , [ Graph.subscribeToGraphUpdates (Urbit.ship session) ]
            )


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

            latestDateNode resource =
                model.graphStore
                    |> Graph.getFromStore resource
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

            status resource =
                case latestDateNode resource of
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
        case model.connectionInfo of
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

            Just { resource } ->
                [ div [ style "margin" "1rem 0" ] [ text (status resource) ]
                , Html.form [ onSubmit Ignore ]
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



-- PORTS


port setupUrbitEventSource : String -> Cmd msg


port onUrbitMessage : (JE.Value -> msg) -> Sub msg
