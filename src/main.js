import {Elm} from './Scheduler.elm'

//##########################################################################################
class FlowIO {
////You can find the names of service names defined in the Web Bluetooth API at
//########################################################################################
//####################---Begin Method definitions inside this function--##################
  instanceNumber;
  isConnected = false;
  bleDevice;
  bleServer;

  constructor(instanceNumber) {
    this.instanceNumber = instanceNumber;
  }

  destroy() {
    if (this.isConnected) {
      this.disconnect()
    }
    delete flowios[this.instanceNumber];
    sendFlowIOsChanged();
  }

  enableControls() {
    try {
      document.querySelector(`#disconnect_btn${this.instanceNumber}`).style.display = "block";
      document.querySelector(`#reconnect_btn${this.instanceNumber}`).style.display = "none";
      document.querySelector(`#loading_btn${this.instanceNumber}`).style.display = "none";
      //Enable all controls that have the class x.
      let xItems = document.getElementsByClassName(`x${this.instanceNumber}`).length;
      for (let j = 0; j < xItems; j++) {
        document.getElementsByClassName(`x${this.instanceNumber}`)[j].disabled = false;
      }
      //Set opacity of the flowiographic if it exists
      if (document.querySelector(`#flowiographic${this.instanceNumber}`)) { //if the flowiographic exists:
        document.querySelector(`#flowiographic${this.instanceNumber}`).style.opacity = 1;
      }
    } catch (e) {
      this.log("Can't enableControls() b/c GUI element not present.");
    }
  }

  disableControls() {
    try {
      document.querySelector(`#reconnect_btn${this.instanceNumber}`).style.display = "block";
      document.querySelector(`#disconnect_btn${this.instanceNumber}`).style.display = "none";
      document.querySelector(`#loading_btn${this.instanceNumber}`).style.display = "none";
      //Disable all controls that have the class x.
      let xItems = document.getElementsByClassName(`x${this.instanceNumber}`).length;
      for (let j = 0; j < xItems; j++) {
        document.getElementsByClassName(`x${this.instanceNumber}`)[j].disabled = true;
      }
      //Set opacity of the flowiographic if it exists
      if (document.querySelector(`#flowiographic${this.instanceNumber}`)) { //if the flowiographic exists:
        document.querySelector(`#flowiographic${this.instanceNumber}`).style.opacity = 0.4;
      }
    } catch (e) {
      this.log("Can't disableControls() b/c GUI element not present.");
    }
  }

  hideReconnectBtn() {
    try {
      document.querySelector(`#reconnect_btn${this.instanceNumber}`).style.display = "none"; //disable reconnect button
    } catch (e) {
      this.log("Can't hideReconnectBtn() b/c GUI element not present.");
    }
  }

  showLoadingBtn() {
    try {
      document.querySelector(`#loading_btn${this.instanceNumber}`).style.display = "block";
    } catch (e) {
      this.log("Can't showLoadingBtn() b/c GUI element not present.");
    }
  }

  log(text) {
    console.log(text);
    document.querySelector(`#log${i}`).textContent = text + '\n' + document.querySelector(`#log${i}`).textContent;
  }

  clearLog() {
    console.log("ClearLog-#" + i);
    document.querySelector(`#log${i}`).textContent = "";
  }

  disconnect() {
    disconnectDevice(this);
  }

  reconnect(attempt = 0) {
    return reconnectDevice(this, attempt);
  }

  initialiseServices() {
    return initializeAllServices(this);
  }
}

let scheduler = Elm.Scheduler.init({node: document.getElementById('scheduler')});

function sendFlowIOsChanged() {
  const values = flowios.map(flowio => ({
    id: flowio.instanceNumber,
    isConnected: flowio.isConnected,
  }))
  console.log("Sending: ", values)
  scheduler.ports.flowIOsChange.send(values)
}
scheduler.ports.executeInstructions.subscribe(function (instructions) {
  // TODO
})

//This file must be loaded first and before any other .js file, because it defines the "listOfServices" array,
//and each service.js file appends its UUID to this array using .push().

//NOTE: The following representations are equivalent:
// myFunction(flowio) and flowio.myFunction()
//The first representation is more convenient for invoking functions from HTML buttons.
//The latter representation is more convernient for invoking from the JavaScript API.
//For the clearLog() function, I have both of those representations for convenience

//whenever you add a new service js file, you only need to update this function:
async function initializeAllServices(flowio) {
  try {
    //await initBatteryService(flowio); //defined in "batteryService.js"
    // await initConfigService(flowio); //defined in "configService.js"
    await initControlService(flowio); //defined in "controlService.js"
    // await initGpioService(flowio); //defined in "gpioService.js"
    // await initIndicatorService(flowio); //defined in "indicatorService.js"
    // await initPowerOffService(flowio); //defined in "powerOffService.js"
    // await initPressureService(flowio); //defined in "pressureService.js"
  } catch (error) {
    console.log("FlowIO error: initializeAllServices() failed :( " + error);
    throw "FlowIO error: initializeAllServices() failed :("
  }
}

//##########################################################################################
//https://googlechrome.github.io/samples/web-bluetooth/characteristic-properties-async-await.html
Window.listOfServices = ['generic_access']; //NEVER modify this, and let each sdrvice push its UUID to it using .push() method.

const DEVICE_NAME_PREFIX = 'FlowIO'; //Allow devices STARTING with this name
let flowios = []; //this will hold all of our flowio object instances.

createNewInstance(); //automatically create the "flowio[0]" instance

function createNewInstance() {
  const i = flowios.length - 1; //index number
  flowios.push(new FlowIO(i));
  //##############################---GUI Methods---#######################################
  //TODO: The following methods contain statements that require certaint GUI/DOM elements to be present.
  //If we create our instance using the API not the GUI, then those elements will not exist, and that will then
  //result in an error. Thus, we must encapsulate the entie function definition inside a try-catch block to avoid
  //errors when using pureley the API. In the future, we want to completely separate the API from the GUI and make the
  //API have no dependency on the GUI.
  sendFlowIOsChanged();
}

//############################---End Method Definitions---########################
//################################################################################
//#############################---Begin Functions---##############################

async function connectDevice(flowio) { //the argument is a SINGLE instance, not an array of instances.
  flowio.showLoadingBtn();
  flowio.hideReconnectBtn();
  let deviceOptions = {filters: [{namePrefix: DEVICE_NAME_PREFIX}], optionalServices: listOfServices};
  //the 'listOfServices' is defined in the conditions.js file.
  try {
    let bleDevice = await navigator.bluetooth.requestDevice(deviceOptions);
    bleDevice.addEventListener('gattserverdisconnected', event => {
      flowio.log("Disconnected from: " + event.target.name + ", " + event.target.id);
      flowio.disableControls(); //disable controls on DISCONNECT EVENT
      sendFlowIOsChanged();
    }); //create and event lisner for disconnect events.
    let bleServer = await bleDevice.gatt.connect();

    //This is how we define IMMUTABLE properties.
    flowio.bleDevice = bleDevice;
    flowio.bleServer = bleServer;
    flowio.isConnected = bleDevice.gatt.connected;
    try {
      await flowio.initialiseServices();
      flowio.enableControls();
      sendFlowIOsChanged();
      flowio.log("\nConnected and Initialized!");
    } catch (error) {
      flowio.log("FlowIO failed to init services :(. The error is" + error)
      flowio.disableControls();
    }
  } catch (error) {
    flowio.log("FlowIO-" + flowio.instanceNumber + " connect request cancelled");
    flowio.disableControls();
  }
}

async function reconnectDevice(flowio, reconnectAttempt = 0) {
  //NOTE: If we reconnect immediately after having disconnected, the reconnection will
  //happen fine, but the service initialization will fail, and then the connection
  //will drop. To fix this problem, we recursively call the reconnect() function
  //in the catch block below until we reconnect successffully or up to 3 tries.
  if (flowio.bleDevice && !flowio.bleDevice.gatt.connected) { //if a device exists but is not connected:
    flowio.log("\nReconnecting...");
    try {
      await flowio.bleDevice.gatt.connect(); //connect to the same bleDevice.
    } catch (error) {
      flowio.log(error);
      flowio.log("The FlowIO device is probably Off");
    }
    if (flowio.bleDevice.gatt.connected) {
      flowio.log("Initializing all services...");
      flowio.showLoadingBtn();
      flowio.hideReconnectBtn();
      try {
        await initializeAllServices(flowio);
        flowio.enableControls();
        flowio.log("ReConnected and Initialized!");
      } catch (error) {
        flowio.log("Reconnect failed. " + error);
        reconnectAttempt++;
        if (reconnectAttempt < 3) {
          flowio.log("Trying again " + reconnectAttempt)
          await reconnectDevice(flowio, reconnectAttempt);
        }
      }
    }
  } else {
    await connectDevice(flowio);
  }
}

function disconnectDevice(flowio) {
  if (!flowio.bleDevice) {
    flowio.log('No device found');
  } else if (flowio.bleDevice.gatt.connected) {
    flowio.log('Disconnecting...');
    flowio.bleDevice.gatt.disconnect();
    // flowio.disableControls(); unnecessary b/c this happens in the disconnect event.
  } else {
    flowio.log('Device already disconnected');
    flowio.disableControls();
  }
  sendFlowIOsChanged();
}

function clearLog(flowio) { //argument is a particular instance
  document.querySelector(`#log${flowio.instanceNumber}`).textContent = "";
}

// function log(text) {
//     console.log(text);
//     document.querySelector(`#log0`).textContent = text + '\n' + document.querySelector(`#log0`).textContent;
// }
