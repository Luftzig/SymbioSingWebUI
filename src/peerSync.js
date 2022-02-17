import io from "socket.io-client"
import SignalingChannel from "./SignalingChannel"

let signalingServer = "https://webrtc-signaling-hxdip2j6ea-lz.a.run.app"
// let signalingServer = "localhost:3030"
const serverToken = "SYMBIOSINGSIGNALING"

let connection;
const connect = (localPeerId) => {
  connection = new SignalingChannel(localPeerId, signalingServer, serverToken)
  connection.connect()
}

export const wire = (elmApp) => {
  elmApp.ports.sendPeerSyncCommand_.subscribe((data) => {
    console.log("from Elm:", data)
    switch (data.command) {
      case "connect":
        if (connection == null) {
          connect(data.peerId)
          connection.onMessage = (message) => {
            console.log("onMessage listener", message)
            elmApp.ports.listenToPeerSync_.send(message)
          }
          console.debug("Connecting to signaling server")
        } else {
          console.debug("Connection already exists")
        }
        return
      case "disconnect":
        if (connection == null) {
          console.debug("already disconnected")
        } else {
          elmApp.ports.listenToPeerSync_.send({type: "disconnected"})
          connection.disconnect()
          connection = undefined
        }
        return
      case "send":
        if (connection == null) {
          console.warn("Trying to send message but not connected")
        } else {
          console.log("send", data)
          connection.send(data)
        }
        return
      case "countdown":
        if (connection == null) {
          console.warn("Trying to send message but not connected")
        } else {
          connection.countdown(data)
        }
        return

      default:
        console.log("Unknown command", data.command)
        return
    }
  })
}
