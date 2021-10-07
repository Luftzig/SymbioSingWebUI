import {Elm} from "./Main.elm"
import {Elm as Scheduler} from "./Scheduler.elm"
import {DEFAULT_SERVICES, FlowIo} from "flow-io-web-lib"
import ControlService from "flow-io-web-lib/src/services/controlService"

const flowIoDevices = []

let app = Elm.Main.init({node: document.getElementById('app-root')});

app.ports.createDevice.subscribe(() => {
  const flowIo = new FlowIo([new ControlService()])
  flowIoDevices.push(flowIo)
})

app.ports.connectToDevice.subscribe(deviceIndex => {
  const device = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to connect to device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.connect({requestedServices: DEFAULT_SERVICES.concat([ControlService.uuid])})
    .then(() => app.ports.deviceStatusChanged.send({deviceIndex, status: 'connected'}))
    .catch((e) => {
      console.log(`Failed to connect to device ${deviceIndex} due to:`, e)
      app.ports.deviceStatusChanged.send({deviceIndex, status: 'disconnected'})
    })
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
