port module PeerSync exposing (PeerSyncCommand(..), sendPeerSyncCommand, PeerSyncMessage(..), listenToPeerSync, PeerSyncState(..))

import Json.Decode
import Json.Encode


port sendPeerSyncCommand_ : Json.Encode.Value -> Cmd msg


port listenToPeerSync_ : (Json.Decode.Value -> msg) -> Sub msg

type PeerSyncState
    = Connected
    | NotConnected


type PeerSyncCommand
    = Connect String
    | Disconnect
    | SendMessage String


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

        SendMessage string ->
            Json.Encode.object
                [ ( "command", Json.Encode.string "send" )
                , ( "message", Json.Encode.string string )
                ]

type PeerSyncMessage
    = Received String

listenToPeerSync : (Result Json.Decode.Error PeerSyncMessage -> msg) -> Sub msg
listenToPeerSync toMessage =
    listenToPeerSync_ (\value -> toMessage (Json.Decode.decodeValue peerSyncMessageDecoder value))

peerSyncMessageDecoder : Json.Decode.Decoder PeerSyncMessage
peerSyncMessageDecoder =
    Json.Decode.map Received Json.Decode.string