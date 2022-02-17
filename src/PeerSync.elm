port module PeerSync exposing (CountdownData, PeerSyncCommand(..), PeerSyncMessage(..), PeerSyncState(..), listenToPeerSync, sendPeerSyncCommand)

import Json.Decode as JD
import Json.Encode


port sendPeerSyncCommand_ : Json.Encode.Value -> Cmd msg


port listenToPeerSync_ : (JD.Value -> msg) -> Sub msg


type PeerSyncState
    = Connected
    | NotConnected


type PeerSyncCommand
    = Connect String
    | Disconnect
    | SendMessage PeerSyncMessage


type PeerSyncMessage
    = Text String
    | Countdown CountdownData
    | PeerReady String
    | Disconnected


type alias CountdownData =
    { count : Int, outOf : Int, intervalMs : Float }


sendPeerSyncCommand : PeerSyncCommand -> Cmd msg
sendPeerSyncCommand command =
    sendPeerSyncCommand_ (encodeSyncPeerCommand command)


encodeSyncPeerCommand : PeerSyncCommand -> Json.Encode.Value
encodeSyncPeerCommand syncPeerCommand =
    case syncPeerCommand of
        Connect name ->
            Json.Encode.object
                [ ( "command", Json.Encode.string "connect" )
                , ( "peerId", Json.Encode.string name )
                ]

        Disconnect ->
            Json.Encode.object [ ( "command", Json.Encode.string "disconnect" ) ]

        SendMessage message ->
            encodeMessage message


encodeMessage : PeerSyncMessage -> Json.Encode.Value
encodeMessage peerSyncMessage =
    case peerSyncMessage of
        Text string ->
            Json.Encode.object <|
                [ ( "command", Json.Encode.string "send" )
                , ( "text", Json.Encode.string string )
                ]

        Countdown record ->
            Json.Encode.object <|
                [ ( "command", Json.Encode.string "countdown" )
                , ( "count", Json.Encode.int record.count )
                , ( "outOf", Json.Encode.int record.outOf )
                , ( "intervalMs", Json.Encode.float record.intervalMs )
                ]

        PeerReady string ->
            Json.Encode.object <|
                [ ( "command", Json.Encode.string "ready" )
                , ( "peer", Json.Encode.string string )
                ]

        Disconnected ->
            -- We shouldn't actually emit this message
            Json.Encode.null



listenToPeerSync : (Result JD.Error PeerSyncMessage -> msg) -> Sub msg
listenToPeerSync toMessage =
    listenToPeerSync_ (\value -> toMessage (JD.decodeValue peerSyncMessageDecoder value))


peerSyncMessageDecoder : JD.Decoder PeerSyncMessage
peerSyncMessageDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\command ->
                case command of
                    "send" ->
                        JD.map Text (JD.field "text" JD.string)

                    "countdown" ->
                        JD.map3 (\count outOf intervalMs -> Countdown { count = count, outOf = outOf, intervalMs = intervalMs })
                            (JD.field "count" JD.int)
                            (JD.field "outOf" JD.int)
                            (JD.field "intervalMs" JD.float)

                    "ready" ->
                        JD.map PeerReady (JD.field "from" JD.string)

                    "disconnected" ->
                        JD.succeed Disconnected

                    string ->
                        JD.fail ("'" ++ string ++ "' is not a supported command. Supported commands are 'send', 'countdown' and 'ready'")
            )
