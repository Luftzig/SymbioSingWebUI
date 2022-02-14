import "./src/style.css"
import "./src/svgbuttons.css"
import {Elm} from "./src/Main.elm"
import {FlowIo} from "flow-io-web-lib"
import ControlService from "flow-io-web-lib/lib/services/controlService"
import {ConfigService} from "flow-io-web-lib/lib/services/configService"
import {PowerOffService} from "flow-io-web-lib/lib/services/powerOffService"
import * as PeerSync from "./src/peerSync"

import LocalStorage from "./src/localStorage"

const flowIoDevices /*: FlowIo[]*/ = []

let app = Elm.Main.init({
  node: document.getElementById('app-root')
  , flags: {width: window.innerWidth, height: window.innerHeight}
});

LocalStorage.wire(app)

app.ports.createDevice.subscribe(() => {
  const flowIo = new FlowIo({
    control: new ControlService(),
    config: new ConfigService(),
    powerOff: new PowerOffService(),
    // analog: new AnalogService(),
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
      return {device, deviceIndex}
    })
    .then(registerServicesToPorts)
    .catch((e) => {
      console.log(`Failed to connect to device ${deviceIndex} due to:`, e)
      app.ports.listenToDeviceStatus.send({deviceIndex, status: 'disconnected', details: null})
    })
})

const registerServicesToPorts = ({device, deviceIndex}) => {
  device.services.config.getConfiguration().then(config =>
    app.ports.listenToDeviceConfiguration_.send({deviceIndex, configuration: config})
  )
  device.services.powerOff.getRemainingTime().then(status =>
    app.ports.listenToPowerOffStatus_.send({deviceIndex, status})
  )
  device.services.powerOff.onStatusChanged(status =>
    app.ports.listenToPowerOffStatus_.send({deviceIndex, status})
  )
  device.services.analog?.onValuesChange(values =>
    app.ports.listenToAnalogReadings_.send({deviceIndex, readings: values}))
}

const getDeviceAndService = (deviceIndex, service) => {
  const device /* : FlowIo */ = flowIoDevices[deviceIndex]
  if (device == null) {
    console.error(`Requested ${service} of device number`, deviceIndex, "but there is no such device. Devices:", flowIoDevices)
    return {device: undefined, service: undefined}
  }
  const srv = device.services[service]
  if (srv == null) {
    console.error(`Requested ${service} of device number`, deviceIndex, `but it has no ${service} service. Available service are:`, Object.keys(device.service))
    return {device: undefined, service: undefined}
  }
  return {device, service: srv}
}

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
  const {service: configService} = getDeviceAndService(deviceIndex, "config")
  if (configService == null) {
    return
  }
  configService.getConfiguration().then(configuration =>
    app.ports.listenToDeviceConfiguration_.send({deviceIndex, configuration})
  )
})

app.ports.sendDeviceConfiguration_.subscribe(({deviceIndex, configuration}) => {
  const {service: configService} = getDeviceAndService(deviceIndex, "config")
  if (configService == null) {
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

  const {service: powerOffService} = getDeviceAndService(deviceIndex, "powerOff")
  if (powerOffService == null) {
    return
  }
  powerOffService.setTimer(convertToArgument(status))
})

/* Analog Service */
app.ports.requestAnalogReadings_.subscribe(({deviceIndex, mode}) => {
  const {service: analogService} = getDeviceAndService(deviceIndex, "analog")
  if (analogService == null) {
    return
  }
  analogService.requestValues(mode.kind, mode.averagingWindowSamples).then(r => {
    console.debug("readings:", r, ". We expect this value to be sent through the event listener.")
  })
})


PeerSync.wire(app)


/*
 * from: https://github.com/wolfadex/wolfadex-components/blob/master/click-outside.js
 * Used for detecting clicks outside of 1 or more elements.
 *
 * Add this script at the top of your HTML file and insert it into your DOM with:
 *
 *     <click-outside id="click-parent">
 *       <!-- children -->
 *     </click-outside>
 *
 * Then in your JS detect clicks outside with:
 *
 *     document
 *       .getElementById("click-parent")
 *       .addEventListener("click-outside", function() {
 *         // Handle click outside here
 *       });
 */
customElements.define(
  "click-outside",
  class extends HTMLElement {
    connectedCallback() {
      window.addEventListener("click", this.clickListener.bind(this));
    }

    disconnectedCallback() {
      window.removeEventListener("click", this.clickListener);
    }

    clickListener(event) {
      if (!this.contains(event.target)) {
        this.dispatchEvent(new CustomEvent("click-outside"));
      }
    }
  }
);