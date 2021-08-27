module Urbit.Metadata exposing
    ( MetadataResource, Config(..), Vip(..)
    , add, remove
    )

{-|

@docs MetadataResource, Config, Vip
@docs add, remove

-}

import Json.Encode as JE
import Time
import Urbit
import Urbit.Encoding.Atom exposing (Atom)
import Urbit.Encoding.DateAbsolute as DateAbsolute exposing (DateRecord)
import Urbit.Encoding.Phonemic as Phonemic
import Urbit.Graph as Graph


{-| A metadata resource
-}
type alias MetadataResource =
    { appName : String
    , resource : Graph.Resource
    }


{-| Metadata config
-}
type Config
    = Group { feed : Maybe MetadataResource }
    | Graph { module_ : String }


encodeConfig : Config -> JE.Value
encodeConfig config =
    case config of
        Group groupConfig ->
            JE.object
                [ ( "group"
                  , case groupConfig.feed of
                        Nothing ->
                            JE.null

                        Just { appName, resource } ->
                            JE.object
                                [ ( "app-name", JE.string appName )
                                , ( "resource"
                                  , JE.string (Graph.resourceToString resource)
                                  )
                                ]
                  )
                ]

        Graph { module_ } ->
            JE.object [ ( "graph", JE.string module_ ) ]


{-| Variation in Permissions

  - `ReaderComments`: Allow readers to comment, regardless of whether they can
    write. (notebook, collections)
  - `MemberMetadata`: Allow members to add channels (groups)
  - `HostFeed`: Only host can post to group feed
  - `AdminFeed`: Only admins and host can post to group feed
  - `NoVariation`: No variation

-}
type Vip
    = ReaderComments
    | MemberMetadata
    | HostFeed
    | AdminFeed
    | NoVariation


vipToString : Vip -> String
vipToString vip =
    case vip of
        ReaderComments ->
            "reader-comments"

        MemberMetadata ->
            "member-metadata"

        HostFeed ->
            "host-feed"

        AdminFeed ->
            "admin-feed"

        NoVariation ->
            ""


{-| Add metadata.

  - `appName` is usually set to `"groups"` for modifying group metadata, and
    `"graph"` for modifying metadata of an individual graph
  - `color` is a hex color code without the "#" (i.e. `"4d0a3b"`)

-}
add :
    { session : Urbit.Session
    , group : Graph.Resource
    , resource : MetadataResource
    , metadata :
        { title : String
        , description : String
        , color : String
        , dateCreated : Time.Posix
        , creator : Atom
        , config : Config
        , picture : String
        , preview : Bool
        , hidden : Bool
        , vip : Vip
        }
    }
    -> Urbit.OutMsg
add { session, group, resource, metadata } =
    let
        meta =
            metadata
    in
    sendMetadataPoke
        { session = session
        , action = "add"
        , group = group
        , appName = resource.appName
        , resource = resource.resource
        , args =
            [ ( "metadata"
              , JE.object
                    [ ( "title", JE.string meta.title )
                    , ( "description", JE.string meta.description )
                    , ( "color", JE.string meta.color )
                    , ( "date-created"
                      , JE.string (DateAbsolute.fromPosix meta.dateCreated)
                      )
                    , ( "creator", JE.string (Phonemic.toPatp meta.creator) )
                    , ( "config", encodeConfig meta.config )
                    , ( "picture", JE.string meta.picture )
                    , ( "preview", JE.bool meta.preview )
                    , ( "hidden", JE.bool meta.hidden )
                    , ( "vip", JE.string (vipToString meta.vip) )
                    ]
              )
            ]
        }


{-| Remove metadata.
-}
remove :
    { session : Urbit.Session
    , group : Graph.Resource
    , resource : MetadataResource
    }
    -> Urbit.OutMsg
remove { session, group, resource } =
    sendMetadataPoke
        { session = session
        , action = "remove"
        , group = group
        , appName = resource.appName
        , resource = resource.resource
        , args = []
        }



-- HELPERS


sendMetadataPoke :
    { session : Urbit.Session
    , action : String
    , group : Graph.Resource
    , appName : String
    , resource : Graph.Resource
    , args : List ( String, JE.Value )
    }
    -> Urbit.OutMsg
sendMetadataPoke { session, action, group, appName, resource, args } =
    Urbit.poke
        { ship = Urbit.ship session
        , app = "metadata-push-hook"
        , mark = "metadata-update-1"
        , json =
            JE.object
                [ ( action
                  , JE.object
                        (( "group", JE.string (Graph.resourceToPath group) )
                            :: ( "resource"
                               , JE.object
                                    [ ( "app-name"
                                      , JE.string appName
                                      )
                                    , ( "resource"
                                      , JE.string <|
                                            Graph.resourceToPath resource
                                      )
                                    ]
                               )
                            :: args
                        )
                  )
                ]
        }
