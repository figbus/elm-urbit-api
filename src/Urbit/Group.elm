module Urbit.Group exposing (createSpider, deleteSpider, leaveSpider, inviteSpider)

{-|

@docs createSpider, deleteSpider, leaveSpider, inviteSpider

-}

import Http
import Json.Decode as JD
import Json.Encode as JE
import Urbit
import Urbit.Encoding.Atom exposing (Atom)
import Urbit.Encoding.Phonemic as Phonemic
import Urbit.Graph as Graph


{-| Create a group via a spider thread.
-}
createSpider :
    { url : String
    , resource : Graph.Resource
    , ships : List Atom
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
createSpider { url, resource, ships } =
    sendGroupSpider
        { url = url
        , groupThread = "create"
        , action = "create"
        , resource = resource
        , args =
            [ ( "ships", ships |> JE.list (Phonemic.toPatp >> JE.string) ) ]
        }


{-| Delete a group via a spider thread.
-}
deleteSpider :
    { url : String
    , resource : Graph.Resource
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
deleteSpider { url, resource } =
    sendGroupSpider
        { url = url
        , groupThread = "delete"
        , action = "remove"
        , resource = resource
        , args = []
        }


{-| Leave a group via a spider thread.
-}
leaveSpider :
    { url : String
    , resource : Graph.Resource
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
leaveSpider { url, resource } =
    sendGroupSpider
        { url = url
        , groupThread = "leave"
        , action = "leave"
        , resource = resource
        , args = []
        }


{-| Send invites to a group via a spider thread.
-}
inviteSpider :
    { url : String
    , resource : Graph.Resource
    , ships : List Atom
    , description : String
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
inviteSpider { url, resource, ships, description } =
    sendGroupSpider
        { url = url
        , groupThread = "invite"
        , action = "invite"
        , resource = resource
        , args =
            [ ( "ships", ships |> JE.list (Phonemic.toPatp >> JE.string) )
            , ( "description", JE.string description )
            ]
        }



-- HELPERS


sendGroupSpider :
    { url : String
    , groupThread : String
    , action : String
    , resource : Graph.Resource
    , args : List ( String, JE.Value )
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
sendGroupSpider { url, groupThread, action, resource, args } =
    Urbit.spider
        { url = url
        , inputMark = "group-view-action"
        , outputMark = "json"
        , threadName = "group-" ++ groupThread
        , body =
            JE.object
                [ ( action
                  , JE.object <|
                        ( "resource", Graph.encodeResource resource )
                            :: args
                  )
                ]
        , decoder = JD.null ()
        }
