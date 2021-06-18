module Urbit.Graph exposing
    ( Store, Resource, emptyStore, getFromStore
    , Graph, Node, getNodeChildren, getNodePost, Post, Index, newNode
    , textContent, urlContent, mentionContent, codeContent
    , getGraph, subscribeToGraphUpdates, addNodes, addNodesSpider
    , createManagedGraph, createUnmanagedGraph, deleteGraphSpider
    , Update, updateDecoder, updateStore
    )

{-|


# Store

@docs Store, Resource, emptyStore, getFromStore


# Graph

@docs Graph, Node, getNodeChildren, getNodePost, Post, Index, newNode
@docs textContent, urlContent, mentionContent, codeContent


# Requests

@docs getGraph, subscribeToGraphUpdates, addNodes, addNodesSpider
@docs createManagedGraph, createUnmanagedGraph, deleteGraphSpider


# Updates

@docs Update, updateDecoder, updateStore

-}

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Time
import Urbit



-- STORE


{-| A collection of [Graphs](#Graph) identified by [Resources](#Resource).
-}
type Store
    = Store (Dict String Graph)


{-| A pair of a ship and a name uniquely identifying a graph.
-}
type alias Resource =
    { ship : String
    , name : String
    }


{-| Create an empty [Store](#Store).
-}
emptyStore : Store
emptyStore =
    Store Dict.empty


{-| Get a graph from a [Store](#Store) using a resource.
-}
getFromStore : Resource -> Store -> Maybe Graph
getFromStore resource (Store store) =
    Dict.get (resourceToKey resource) store



-- GRAPH


{-| An ordered dict of nodes.
-}
type alias Graph =
    Dict String Node


{-| A node in a graph. Contains a [Post](#Post) and possibly children.
-}
type Node
    = Node Post Graph


{-| Get the post associated with a node.
-}
getNodePost : Node -> Post
getNodePost (Node post _) =
    post


{-| Get the children of a node.
-}
getNodeChildren : Node -> Graph
getNodeChildren (Node _ children) =
    children


{-| A post in a graph. The contents are a list of JSON values.
-}
type alias Post =
    { index : Index
    , author : String
    , timeSent : Time.Posix
    , contents : List JE.Value
    , hash : Maybe String
    }


{-| An index uniquely identifying a node within a graph.
-}
type alias Index =
    List String


{-| Create a new node from a post.
-}
newNode : Post -> Node
newNode post =
    Node post Dict.empty


{-| Create a `text` content type for a post.
-}
textContent : String -> JE.Value
textContent text =
    JE.object [ ( "text", JE.string text ) ]


{-| Create a `url` content type for a post.
-}
urlContent : String -> JE.Value
urlContent url =
    JE.object [ ( "url", JE.string url ) ]


{-| Create a `mention` content type for a post.
-}
mentionContent : String -> JE.Value
mentionContent referencedShip =
    JE.object [ ( "mention", JE.string referencedShip ) ]


{-| Create a `code` content type for a post.
-}
codeContent : { expression : String, output : String } -> JE.Value
codeContent { expression, output } =
    JE.object
        [ ( "code"
          , JE.object
                [ ( "expression", JE.string expression )
                , ( "output"
                  , JE.list identity [ JE.list identity [ JE.string output ] ]
                  )
                ]
          )
        ]



-- REQUESTS


{-| Get a graph from the graph store.
-}
getGraph :
    { url : String
    , resource : Resource
    }
    -> (Result Http.Error Update -> msg)
    -> Cmd msg
getGraph { url, resource } =
    Urbit.scry
        { url = url
        , app = "graph-store"
        , path = "/graph/" ++ resource.ship ++ "/" ++ resource.name
        , mark = "json"
        , decoder = updateDecoder
        }


{-| Subscribe to changes to the graph store.
-}
subscribeToGraphUpdates :
    { ship : String
    , session : Urbit.Session
    }
    -> (Result Http.Error () -> msg)
    -> ( Urbit.Session, Cmd msg )
subscribeToGraphUpdates { ship, session } =
    Urbit.subscribe
        { ship = ship
        , app = "graph-store"
        , path = "/updates"
        , session = session
        }


{-| Add nodes to the graph store.
-}
addNodes :
    { resource : Resource
    , nodes : List Node
    , session : Urbit.Session
    }
    -> (Result Http.Error () -> msg)
    -> ( Urbit.Session, Cmd msg )
addNodes { resource, nodes, session } =
    Urbit.poke
        { ship = Urbit.ship session
        , app = "graph-push-hook"
        , mark = "graph-update-2"
        , json = encodeAddNodesGraphUpdate resource nodes
        , session = session
        }


{-| Add nodes to the graph store via a spider thread.
-}
addNodesSpider :
    { url : String
    , resource : Resource
    , nodes : List Node
    }
    -> (Result Http.Error JD.Value -> msg)
    -> Cmd msg
addNodesSpider { url, resource, nodes } =
    Urbit.spider
        { url = url
        , inputMark = "graph-update-2"
        , outputMark = "graph-view-action"
        , threadName = "graph-add-nodes"
        , body = encodeAddNodesGraphUpdate resource nodes
        }


{-| Create a new graph associated with an existing group.
-}
createManagedGraph :
    { url : String
    , group : Resource
    , resource : Resource
    , title : String
    , description : String
    , graphModule : String
    , mark : String
    }
    -> (Result Http.Error JD.Value -> msg)
    -> Cmd msg
createManagedGraph config =
    Urbit.spider
        { url = config.url
        , inputMark = "graph-view-action"
        , outputMark = "json"
        , threadName = "graph-create"
        , body =
            encodeGraphCreate
                { resource = config.resource
                , title = config.title
                , description = config.description
                , associated =
                    JE.object [ ( "group", encodeResource config.group ) ]
                , graphModule = config.graphModule
                , mark = config.mark
                }
        }


{-| Create a new graph unassociated with any group.
-}
createUnmanagedGraph :
    { url : String
    , resource : Resource
    , title : String
    , description : String
    , invites : List String
    , graphModule : String
    , mark : String
    }
    -> (Result Http.Error JD.Value -> msg)
    -> Cmd msg
createUnmanagedGraph config =
    Urbit.spider
        { url = config.url
        , inputMark = "graph-view-action"
        , outputMark = "json"
        , threadName = "graph-create"
        , body =
            encodeGraphCreate
                { resource = config.resource
                , title = config.title
                , description = config.description
                , associated =
                    JE.object
                        [ ( "policy"
                          , JE.object
                                [ ( "invite"
                                  , JE.object
                                        [ ( "pending"
                                          , JE.list JE.string config.invites
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]
                , graphModule = config.graphModule
                , mark = config.mark
                }
        }


{-| Delete a graph from the graph store via a spider thread.
-}
deleteGraphSpider :
    { url : String
    , resource : Resource
    }
    -> (Result Http.Error JD.Value -> msg)
    -> Cmd msg
deleteGraphSpider { url, resource } =
    Urbit.spider
        { url = url
        , inputMark = "graph-view-action"
        , outputMark = "json"
        , threadName = "graph-delete"
        , body =
            JE.object
                [ ( "delete"
                  , JE.object [ ( "resource", encodeResource resource ) ]
                  )
                ]
        }



-- UPDATES


{-| Represents a change to the graph store.
-}
type Update
    = AddGraph Resource Graph
    | AddNodes Resource (List ( Index, Node ))
    | RemoveNodes Resource (List Index)
    | RemoveGraph Resource


{-| Apply a change to the graph store.
-}
updateStore : Update -> Store -> Store
updateStore graphUpdate (Store store) =
    Store <|
        case graphUpdate of
            AddGraph resource newGraph ->
                Dict.insert (resourceToKey resource) newGraph store

            AddNodes resource newNodes ->
                let
                    insertNode ( index, node ) graph =
                        case index of
                            [] ->
                                graph

                            [ segment ] ->
                                Dict.insert segment node graph

                            head :: tail ->
                                Dict.update head
                                    (Maybe.map
                                        (\(Node post subGraph) ->
                                            subGraph
                                                |> insertNode ( tail, node )
                                                |> Node post
                                        )
                                    )
                                    graph
                in
                Dict.update (resourceToKey resource)
                    (Maybe.withDefault Dict.empty
                        >> (\graph -> List.foldl insertNode graph newNodes)
                        >> Just
                    )
                    store

            RemoveNodes resource indices ->
                Dict.update (resourceToKey resource)
                    (Maybe.map
                        (\graph ->
                            List.foldl
                                (\index graph_ ->
                                    case index of
                                        [ indexPart ] ->
                                            Dict.remove indexPart graph_

                                        _ ->
                                            graph_
                                )
                                graph
                                indices
                        )
                    )
                    store

            RemoveGraph resource ->
                Dict.remove (resourceToKey resource) store



-- JSON DECODERS


{-| Decode a graph store update, typically from an incoming diff or scry
request.
-}
updateDecoder : JD.Decoder Update
updateDecoder =
    let
        resourceDecoder =
            JD.field "resource" <|
                JD.map2 Resource
                    (JD.field "ship" JD.string |> JD.map ((++) "~"))
                    (JD.field "name" JD.string)
    in
    JD.field "graph-update" <|
        JD.oneOf
            [ JD.field "add-graph" <|
                JD.map2 AddGraph
                    resourceDecoder
                    graphDecoder
            , JD.field "add-nodes" <|
                JD.map2 AddNodes
                    resourceDecoder
                    (JD.field "nodes" <|
                        (JD.keyValuePairs nodeDecoder
                            |> JD.map (List.map (Tuple.mapFirst parseIndex))
                        )
                    )
            , JD.field "remove-nodes" <|
                JD.map2 RemoveNodes
                    resourceDecoder
                    (JD.field "indices" <|
                        JD.list (JD.string |> JD.map parseIndex)
                    )
            ]


graphDecoder : JD.Decoder Graph
graphDecoder =
    JD.field "graph"
        (JD.keyValuePairs nodeDecoder
            |> JD.map Dict.fromList
        )


nodeDecoder : JD.Decoder Node
nodeDecoder =
    JD.map2 Node
        (JD.field "post" <|
            JD.map5 Post
                (JD.field "index" JD.string |> JD.map parseIndex)
                (JD.field "author" JD.string)
                (JD.field "time-sent" JD.int |> JD.map Time.millisToPosix)
                (JD.field "contents" <| JD.list JD.value)
                (JD.field "hash" <| JD.maybe JD.string)
        )
        (JD.field "children" (JD.maybe (JD.lazy (\_ -> graphDecoder)))
            |> JD.map (Maybe.withDefault Dict.empty)
        )



-- JSON ENCODERS


encodeAddNodesGraphUpdate : Resource -> List Node -> JE.Value
encodeAddNodesGraphUpdate resource nodes =
    JE.object
        [ ( "add-nodes"
          , JE.object
                [ ( "resource", encodeResource resource )
                , ( "nodes", encodeNodes nodes )
                ]
          )
        ]


encodeGraphCreate :
    { resource : Resource
    , title : String
    , description : String
    , associated : JE.Value
    , graphModule : String
    , mark : String
    }
    -> JE.Value
encodeGraphCreate config =
    JE.object
        [ ( "create"
          , JE.object
                [ ( "resource", encodeResource config.resource )
                , ( "title", JE.string config.title )
                , ( "description", JE.string config.description )
                , ( "associated", config.associated )
                , ( "module", JE.string config.graphModule )
                , ( "mark", JE.string config.mark )
                ]
          )
        ]


encodeResource : Resource -> JE.Value
encodeResource resource =
    JE.object
        [ ( "ship", JE.string resource.ship )
        , ( "name", JE.string resource.name )
        ]


encodeNodes : List Node -> JE.Value
encodeNodes =
    List.map
        (\((Node post _) as node) ->
            ( encodeIndex post.index
            , encodeNode node
            )
        )
        >> JE.object


encodeNode : Node -> JE.Value
encodeNode (Node post children) =
    JE.object
        [ ( "post"
          , JE.object
                [ ( "index", JE.string <| encodeIndex post.index )
                , ( "author", JE.string post.author )
                , ( "time-sent", JE.int <| Time.posixToMillis post.timeSent )
                , ( "signatures", JE.list identity [] )
                , ( "contents", JE.list identity post.contents )
                , ( "hash", JE.null )
                ]
          )
        , ( "children"
          , children
                |> Dict.toList
                |> List.map (Tuple.mapSecond encodeNode)
                |> JE.object
          )
        ]


encodeIndex : Index -> String
encodeIndex index =
    "/" ++ String.join "/" index



-- INTERNAL HELPERS


parseIndex : String -> List String
parseIndex =
    String.split "/"
        >> List.tail
        >> Maybe.withDefault []


resourceToKey : Resource -> String
resourceToKey { ship, name } =
    ship ++ name
