import {Elm} from "./Main.elm"
import {DEFAULT_SERVICES, FlowIo} from "flow-io-web-lib"
import ControlService from "flow-io-web-lib/src/services/controlService"
import {ConfigService} from "flow-io-web-lib/lib/services/configService"
import {BatteryService} from "flow-io-web-lib/lib/services/batteryService"
import {PowerOffService} from "flow-io-web-lib/lib/services/powerOffService"

const flowIoDevices /*: FlowIo[]*/ = []

let app = Elm.Main.init({node: document.getElementById('app-root')});

app.ports.createDevice.subscribe(() => {
  const flowIo = new FlowIo({
    control: new ControlService(),
    config: new ConfigService(),
    powerOff: new PowerOffService()
  })
  flowIoDevices.push(flowIo)
})

app.ports.connectToDevice.subscribe(deviceIndex => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to connect to device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.connect()
    .then(() => {
      const deviceConnectionStatus = {
        deviceIndex,
        status: 'connected',
        details: {name: device.name, id: device.id, services: Object.values(device.services).map(s => s.id)}
      }
      app.ports.listenToDeviceStatus.send(deviceConnectionStatus)
      device.connection.subscribe("disconnected", (_) => {
        app.ports.listenToDeviceStatus.send({
          deviceIndex,
          status: 'disconnected',
          details: null
        })
      })
    })
    .then(() => {
      device.services.config.getConfiguration().then(config =>
        app.ports.listenToDeviceConfiguration_.send({deviceIndex, configuration: config})
      )
      device.services.powerOff.getRemainingTime().then(status =>
        app.ports.listenToPowerOffStatus_.send({deviceIndex, status})
      )
      device.services.powerOff.onStatusChanged(status =>
        app.ports.listenToPowerOffStatus_.send({deviceIndex, status})
      )
    })
    .catch((e) => {
      console.log(`Failed to connect to device ${deviceIndex} due to:`, e)
      app.ports.listenToDeviceStatus.send({deviceIndex, status: 'disconnected', details: null})
    })
})

/* Control Service */
app.ports.listenToControlService.subscribe(deviceIndex => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to listen on device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.services.control.onStatusUpdated(status =>
    app.ports.listenToDeviceControlStatus.send({deviceIndex, status})
  )
})

app.ports.sendCommand.subscribe(({deviceIndex, command}) => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to send command to device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.services.control.sendCommand(command)
    .then(_ => console.debug("Command", command, "sent to device", deviceIndex))
})

app.ports.sendStopAll.subscribe((deviceIndex) => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested to send command to device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  device.services.control.stopAllActions()
    .then(_ => console.debug("Stop all sent to device", deviceIndex))
})

/* Configuration Service */

app.ports.queryDeviceConfiguration.subscribe(deviceIndex => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested configuration of device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  const configService = device.services.config
  if (configService == null) {
    console.error("Requested configuration of device number", deviceIndex, "but it has no configuration service." +
      " Available service are:", Object.keys(device.service))
    return
  }
  configService.getConfiguration().then(configuration =>
    app.ports.listenToDeviceConfiguration_.send({deviceIndex, configuration})
  )
})

app.ports.sendDeviceConfiguration_.subscribe(({deviceIndex, configuration}) => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested configuration of device number", deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return
  }
  const configService = device.services.config
  if (configService == null) {
    console.error("Requested to set configuration of device number", deviceIndex, "but it has no configuration" +
      " service." +
      " Available service are:", Object.keys(device.service))
    return
  }
  configService.setConfiguration(configuration)
    .then(() => configService.getConfiguration())
    .then(configuration =>
      app.ports.listenToDeviceConfiguration_.send({deviceIndex, configuration})
    )
})

/* Power off service */
app.ports.sendPowerOffStatus_.subscribe(({deviceIndex, status}) => {
  function convertToArgument(newStatus /* : PowerOffStatus */) {
    switch (newStatus.kind) {
      case "off":
        return "off"
      case "disabled":
        return "disabled"
      case "remaining":
        return newStatus.minutes
    }
  }

  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error("Requested power-off service of device number", deviceIndex, "but there is no such device." +
      " Devices:", flowIoDevices)
    return
  }
  const powerOffService /* : PowerOffService | undefined */ = device.services.powerOff
  if (powerOffService == null) {
    console.error("Requested to set power-off service of device number", deviceIndex, "but it has no power-off" +
      " service." +
      " Available service are:", Object.keys(device.service))
    return
  }
  powerOffService.setTimer(convertToArgument(status))
})