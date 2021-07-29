# Elm Urbit HTTP API

[![](https://img.shields.io/badge/~-awesome%20urbit-lightgrey)](https://github.com/urbit/awesome-urbit)

This is an Elm wrapper for the [Urbit](https://urbit.org/) HTTP API, making it
easy to interact with Urbit from any Elm application.

## Examples

```elm
import Random
import Time
import Json.Encode
import Urbit

connect : Int -> Time.Posix -> Cmd Msg
connect entropy time =
    Urbit.connect
        { ship = zod
        , url = "http://localhost:8080"
        , channelId =
            Random.initialSeed entropy
                |> Random.step (Urbit.channelId time)
                |> Tuple.first
        , code = "lidlut-tabwed-pillex-ridrup"
        }
        GotConnection

poke : Urbit.Session -> ( Urbit.Session, Cmd Msg )
poke session =
    Urbit.poke
        { ship = zod
        , app = "hood"
        , mark = "helm-hi"
        , json = Json.Encode.string "opening airlock"
        }
        |> Urbit.send session PokedHood
```

See the `example/` folder for a more complete example.

## Ports Disclaimer

Because Elm does not support the [EventSource](https://developer.mozilla.org/en-US/docs/Web/API/EventSource)
API, some additional work is required to create the ports necessary for most
functionality. See the `Urbit.setupEventSource` documentation for details.
