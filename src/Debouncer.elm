-- Also look at:
-- https://package.elm-lang.org/packages/jinjor/elm-debounce/latest/
-- https://package.elm-lang.org/packages/pd-andy/elm-limiter/latest/Limiter
module Debouncer exposing (..)

module Limiter exposing
    ( Msg
    , throttled, throttleAfter, debounce, manual, manualAfter
    , Send, onSettle
    , unlock
    , Limited, Limiter, Wrapper, latest
    )

{-| The Modelr. See the full example [here](https://github.com/jinjor/elm-debounce/blob/master/examples/Main.elm).

  - This module works with the Elm Architecture.
  - You can choose the strategy and define how commands are sent.


# Types

@docs Model, Msg


# Initialize

@docs throttled, throttleAfter, debounce, manual, manualAfter


# Sending Commands

@docs Send, onSettle, withAccumulated


# Update

@docs update, unlock


# View

@docs msg, head, all

-}

import Process
import Task
import Util.NonEmpty.List as Nel exposing (Nel)


type alias Limiter msg value =
    { init : Limited value
    , update : Send value msg -> Msg value -> Limited value -> ( Limited value, Cmd msg )

    -- evented, onEvent, on, msg, constructor... ???
    , strategy : value -> msg
    , unlock : Limited value -> Cmd msg
    }


{-| The state of the debouncer.

It is parameterized with the value type `a`.

-}
type Limited value
    = Model
        { input : Nel value
        , busy : Bool
        }


{-| Strategy defines the timing when commands are sent.
-}
type alias Strategy msg value =
    value -> msg


{-| Send command as soon as it gets ready, with given rate limit.

Note: The first command will be sent immidiately.

-}
throttled : Int -> Wrapper msg value -> value -> Limiter msg value
throttled delay wrap =
    init (wrap << GotThrottleMsg 0 (FromThrottle (Just (abs delay)))) wrap


{-| Similar to `throttled`, but the first command is sent after offset time.
-}
throttleAfter : Int -> Int -> Wrapper msg value -> value -> Limiter msg value
throttleAfter offset delay wrap =
    init (wrap << GotThrottleMsg (abs offset) (FromThrottle (Just (abs delay)))) wrap


{-| Send command after becomming stable, with given delay time.
-}
debounce : Int -> Wrapper msg value -> value -> Limiter msg value
debounce offset wrap =
    init (wrap << GotDebounceMsg (abs offset)) wrap


{-| Send command as soon as it gets ready, but not again until it gets unlocked manually. See `unlock`.

Typically, `unlock` is called after previous response comes back.

-}
manual : Wrapper msg value -> value -> Limiter msg value
manual wrap =
    init (wrap << GotThrottleMsg 0 (FromThrottle Nothing)) wrap


{-| Similar to `manual`, but the first command is sent after offset time.
-}
manualAfter : Int -> Wrapper msg value -> value -> Limiter msg value
manualAfter offset wrap =
    init (wrap << GotThrottleMsg (abs offset) (FromThrottle Nothing)) wrap


{-| Initialize the debouncer. Call this from your `init` function.
-}
init : Strategy msg value -> Wrapper msg value -> value -> Limiter msg value
init strategy wrap def =
    { init = Model { input = ( def, [] ), busy = False }
    , update = update wrap
    , strategy = strategy
    , unlock = unlock wrap
    }


{-| This is the component's update function following the Elm Architecture.

e.g. Saving the last value.

    ( debounce, cmd ) =
        model.eventedValue
            |> Model.update (Model.onSettle Save) msg
            |> Tuple.mapFirst (\new -> { model | eventedValue = new })

The config should be constant and shared with `msg` function.

The sending logic can depend on the current model. If you want to stop sending, return `Cmd.none`.

-}
update : Wrapper msg value -> Send value msg -> Msg value -> Limited value -> ( Limited value, Cmd msg )
update wrap send msg ((Model d) as old) =
    case msg of
        FromThrottle tryAgainAfter ->
            case d.input of
                ( head, _ :: _ ) as accumulated ->
                    let
                        selfCmd : Cmd msg
                        selfCmd =
                            case tryAgainAfter of
                                Just delay ->
                                    sleep delay wrap (FromThrottle (Just delay))

                                _ ->
                                    Cmd.none
                    in
                    ( Model { d | input = ( head, [] ), busy = True }
                    , Cmd.batch [ send accumulated, selfCmd ]
                    )

                _ ->
                    ( Model { d | busy = False }, Cmd.none )

        GotThrottleMsg offset msg_ v ->
            ( Model { d | input = Nel.cons v d.input }
            , if d.busy then
                Cmd.none

              else
                sleep offset wrap msg_
            )

        GotDebounceMsg offset v ->
            ( Model { d | input = Nel.cons v d.input }
            , sleep offset wrap (FromDebounce v)
            )

        FromDebounce lastInput ->
            case ( Nel.head d.input == lastInput, d.input ) of
                ( True, ( head, _ ) ) ->
                    ( Model { d | input = ( head, [] ) }, send d.input )

                _ ->
                    let
                        noop : ( Limited value, Cmd msg )
                        noop =
                            ( old, Cmd.none )
                    in
                    noop


latest : Limited value -> value
latest (Model d) =
    Nel.head d.input


{-| This function consumes values and send a command.

If you want to postpone sending, return the values back to keep them.

-}
type alias Send a msg =
    Nel a -> Cmd msg


{-| Send a command using the accumulated values.
-}
onSettle : (Nel a -> msg) -> Send a msg
onSettle fn nel =
    Task.perform identity (Task.succeed (fn nel))


{-| The messages that are used internally.
-}
type Msg value
    = GotThrottleMsg Int (Msg value) value
    | FromThrottle (Maybe Int)
    | GotDebounceMsg Int value
    | FromDebounce value


type alias Wrapper msg value =
    Msg value -> msg


{-| Manually unlock. This works for `manual` or `manualAfter` Strategy.
-}
unlock : Wrapper msg value -> Limited value -> Cmd msg
unlock wrap _ =
    FromThrottle Nothing
        |> Task.succeed
        |> Task.perform wrap



-- HELPERS


sleep : Int -> (Msg value -> msg) -> Msg value -> Cmd msg
sleep offset wrap msg =
    Task.perform (always (wrap msg)) (Process.sleep (toFloat offset))

--
import Limiter exposing (Limited)

type Msg = OnEdit (Limiter.Msg String)
type alias Model = { debouncedInput : Limited String }

debouncedInput : Limiter Msg String
debouncedInput =
   Limiter.debounce 250 OnEdit ""

init = { debouncedInput = debouncedInput.init }

-- update

   OnEdit limiterMsg ->
       model.debouncedInput
       |> debouncedInput.update
           (Limiter.onSettle (Nel.head >> FetchRecordsFromServer)
           limiterMsg

-- ... later in you're view code...

let
   currentValue = Limiter.latest model.debouncedInput
in
   TextField.input currentValue {onChange = debouncedInput.strategy}