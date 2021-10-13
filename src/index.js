import {Elm} from "./Main.elm"
import {Elm as Scheduler} from "./Scheduler.elm"
import {DEFAULT_SERVICES, FlowIo} from "flow-io-web-lib"
import ControlService from "flow-io-web-lib/src/services/controlService"

const flowIoDevices /*: FlowIo[]*/ = []

let app = Elm.Main.init({node: document.getElementById('app-root')});

app.ports.createDevice.subscribe(() => {
  const flowIo = new FlowIo([new ControlService()])
  flowIoDevices.push(flowIo)
})

app.ports.connectToDevice.subscribe(deviceIndex => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to connect to device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.connect({requestedServices: DEFAULT_SERVICES.concat([ControlService.uuid])})
    .then(() => {
      app.ports.deviceStatusChanged.send({
        deviceIndex,
        status: 'connected',
        details: {name: device.name, id: device.id, services: Object.keys(device.services)}
      })
      device.connection.subscribe("disconnected", (_) => {
        app.ports.deviceStatusChanged.send({
          deviceIndex,
          status: 'disconnected',
          details: null
        })
      })
    })
    .catch((e) => {
      console.log(`Failed to connect to device ${deviceIndex} due to:`, e)
      app.ports.deviceStatusChanged.send({deviceIndex, status: 'disconnected', details: null})
    })
})

app.ports.listenToControlService.subscribe(deviceIndex => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to listen on device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.services[ControlService.name].onStatusUpdated(status =>
    app.ports.controlServiceStatusChanged.send({deviceIndex, status})
  )
})

app.ports.sendCommand.subscribe(( {deviceIndex, command} ) => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to send command to device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.services[ControlService.name].sendCommand(command)
})

// function sendFlowIOsChanged() {
//   const values = flowios.map(flowio => ({
//     id: flowio.instanceNumber,
//     isConnected: flowio.isConnected,
//   }))
//   console.log("Sending: ", values)
//   scheduler.ports.flowIOsChange.send(values)
// }
// scheduler.ports.executeInstructions.subscribe(function (instructions) {
//   // TODO
// })
