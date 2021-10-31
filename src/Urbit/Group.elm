module Urbit.Group exposing
    ( Policy(..), Ban(..)
    , createSpider, deleteSpider, leaveSpider, inviteSpider
    )

{-|

@docs Policy, Ban
@docs createSpider, deleteSpider, leaveSpider, inviteSpider

-}

import Http
import Json.Decode as JD
import Json.Encode as JE
import Urbit
import Urbit.Encoding.Atom exposing (Atom)
import Urbit.Encoding.Phonemic as Phonemic
import Urbit.Graph as Graph


{-| Policy for a group.

  - `Invite` will set the group to be invite-only
      - The `pending` field is the set of ships to send invites to
  - `Open` will set the group to be generally open to the public
      - The `banRanks` field allows you to ban entire classes of ships. See
        <https://urbit.org/docs/glossary/ship> for the meanings of ship ranks.
      - The `banned` field allows you to ban specific ships

-}
type Policy
    = Invite { pending : List Atom }
    | Open
        { banRanks :
            { galaxies : Ban
            , stars : Ban
            , planets : Ban
            , moons : Ban
            , comets : Ban
            }
        , banned : List Atom
        }


{-| Banned or allowed.
-}
type Ban
    = Banned
    | Allowed


encodePolicy : Policy -> JE.Value
encodePolicy policy =
    case policy of
        Invite { pending } ->
            JE.object
                [ ( "invite"
                  , JE.object
                        [ ( "pending"
                          , pending |> JE.list (Phonemic.toPatp >> JE.string)
                          )
                        ]
                  )
                ]

        Open { banRanks, banned } ->
            let
                addIfBanned : Ban -> String -> List String -> List String
                addIfBanned ban string bans =
                    case ban of
                        Banned ->
                            string :: bans

                        Allowed ->
                            bans
            in
            JE.object
                [ ( "open"
                  , JE.object
                        [ ( "ban-ranks"
                          , []
                                |> addIfBanned banRanks.galaxies "czar"
                                |> addIfBanned banRanks.stars "king"
                                |> addIfBanned banRanks.planets "duke"
                                |> addIfBanned banRanks.moons "earl"
                                |> addIfBanned banRanks.comets "pawn"
                                |> JE.list JE.string
                          )
                        , ( "banned"
                          , banned |> JE.list (Phonemic.toPatp >> JE.string)
                          )
                        ]
                  )
                ]


{-| Create a group via a spider thread.
-}
createSpider :
    { url : String
    , name : String
    , policy : Policy
    , title : String
    , description : String
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
createSpider { url, name, policy, title, description } =
    sendGroupSpider
        { url = url
        , groupThread = "create"
        , action = "create"
        , args =
            JE.object
                [ ( "name", JE.string name )
                , ( "policy", encodePolicy policy )
                , ( "title", JE.string title )
                , ( "description", JE.string description )
                ]
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
        , args = Graph.encodeResource resource
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
        , args = Graph.encodeResource resource
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
        , args =
            JE.object
                [ ( "resource", Graph.encodeResource resource )
                , ( "ships", ships |> JE.list (Phonemic.toPatp >> JE.string) )
                , ( "description", JE.string description )
                ]
        }



-- HELPERS


sendGroupSpider :
    { url : String
    , groupThread : String
    , action : String
    , args : JE.Value
    }
    -> (Result Http.Error () -> msg)
    -> Cmd msg
sendGroupSpider { url, groupThread, action, args } =
    Urbit.spider
        { url = url
        , desk = "landscape"
        , inputMark = "group-view-action"
        , outputMark = "json"
        , threadName = "group-" ++ groupThread
        , body = JE.object [ ( action, args ) ]
        , decoder = JD.null ()
        }
