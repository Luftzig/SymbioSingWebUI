/* The ascii character hex equivalencies are:
  ! = 0x21 -- stop
  + = 0x2b -- inflation
  - = 0x2d -- vacuum
  ^ = 0x5e -- release
  ? = 0x3f -- pressure
  p = 0x70
  n = 0x6e

  This API has the following methods and constants:

  flowios[0].controlService
  flowios[0].controlService.chrCommand
  flowios[0].controlService.chrHardwareStatus

  flowios[0].status
  flowios[0].status.active
  flowios[0].status.hardwareStatus
  flowios[0].status.pump1
  flowios[0].status.pump2
  flowios[0].status.inlet
  flowios[0].status.outlet
  flowios[0].status.port1
  flowios[0].status.port2
  flowios[0].status.port3
  flowios[0].status.port4
  flowios[0].status.port5

  flowios[0].getHardwareStatus()
  flowios[0].writeCommand(what,where,how=MAXPWM)
  flowios[0].commandArray[what,where,how]
  flowios[0].startInflation(where,how=flowio.pump1PWM)
  flowios[0].startVacuum (where,how=flowio.pump2PWM)
  flowios[0].startRelease(where)
  flowios[0].stopAction(where)
  flowios[0].stopAllActions()
  flowios[0].setPump1PWM(pwmValue)
  flowios[0].setPump2PWM(pwmValue)
*/
const STOP = 0x21;
const INFLATION = 0x2b;
const VACUUM = 0x2d;
const RELEASE = 0x5e;
const INFLATION_HALF = 0x70;
const VACUUM_HALF = 0x6e;
//------------
const ALLPORTS = 0xff;
const MAXPWM = 0xff;
//-----------------------------------------------------
'use strict'
const controlServiceUUID    = '0b0b0b0b-0b0b-0b0b-0b0b-00000000aa04';
const chrCommandUUID        = '0b0b0b0b-0b0b-0b0b-0b0b-c1000000aa04';
const chrHardwareStatusUUID = '0b0b0b0b-0b0b-0b0b-0b0b-c2000000aa04';

Window.listOfServices.push(controlServiceUUID); //appends this service to the array (defined in conditions.js).

async function initControlService(flowio){
  try{
    //NOTE: If we make these immutable, we can't have the reconnect feature becase we must reinvoke this function on reconnect.
    flowio.controlService = await flowio.bleServer.getPrimaryService(controlServiceUUID);
    flowio.controlService.chrCommand = await flowio.controlService.getCharacteristic(chrCommandUUID);
    flowio.controlService.chrHardwareStatus = await flowio.controlService.getCharacteristic(chrHardwareStatusUUID);
    //subscribe to receive characteristic notification events:
    await flowio.controlService.chrHardwareStatus.startNotifications();
    flowio.controlService.chrHardwareStatus.addEventListener('characteristicvaluechanged', event => { //an event is returned
      //We want all status object-variables to be changed in the event listener, as soon as they
      //change in the hardware. Not inside the getHardwareStatus() function because there may be
      //a chagne in the hardware status even if we don't invoke this function.
      flowio.status = new Object(); //we must define this as an object in order to be able to have children to it.
      flowio.status.hardwareStatus = event.target.value.getUint16(0,true).toBinaryString(16); //true causes the endicanness to be correct.
      let byte0 = event.target.value.getUint8(0);
      let byte1 = event.target.value.getUint8(1);
      flowio.status.pump1 = (byte0>>7 & 0x01);
      flowio.status.pump2 = (byte1 & 0x01);
      flowio.status.inlet = (byte0>>5 & 0x01);
      flowio.status.outlet = (byte0>>6 & 0x01);
      flowio.status.port1 = (byte0>>0 & 0x01);
      flowio.status.port2 = (byte0>>1 & 0x01);
      flowio.status.port3 = (byte0>>2 & 0x01);
      flowio.status.port4 = (byte0>>3 & 0x01);
      flowio.status.port5 = (byte0>>4 & 0x01);

      //Create a status active / inactive flag that we can later use in our pressure service when choosing if a pressure value should be assigned to a port.
      if(byte0==0) flowio.status.active=false;
      else flowio.status.active=true;

      setSvgStatus(flowio);
    });

    //########################--- Define API Methods ---######################
    flowio.getHardwareStatus = async function(){
      await flowio.controlService.chrHardwareStatus.readValue(); //this returns a DataView
      //we don't need to store or process this, because it causes our event litener to be fired.
    }
    //Most important method in the entire API
    flowio.writeCommand = async function(what,where,how=MAXPWM){
      //TODO: Make the commandArray 4-bytes after you change the communication protocol to be 4-bytes.
      flowio.commandArray = new Uint8Array([what,where,how]); //Always holds the last command written.
      //All action methods are in terms of the writeCommand() method so this is updated automatically.
      //if the third byte is 255, then we are going to send only the first 2bytes to the FlowIO to save time and bandwidth.
      if(flowio.commandArray[2]==255){ //in this case only send an array of 2-bytes.
        let array2byte = new Uint8Array([what, where]);
        await flowio.controlService.chrCommand.writeValue(array2byte);
      }
      else{
        await flowio.controlService.chrCommand.writeValue(flowio.commandArray);
      }
    }

    //TODO: After I start using the 4-byte protocol, I should add a 4th optional argument to the action methods.
    //TODO: Add the halfcapacity functions to the API.
    flowio.startInflation = async function(where,how=flowio.pump1PWM){ //set default value to the pump1pwm flag.
      await flowio.writeCommand(INFLATION,where,how);
    }
    flowio.startVacuum = async function(where,how=flowio.pump2PWM){
      await flowio.writeCommand(VACUUM,where,how);
    }
    flowio.startRelease = async function(where){
      await flowio.writeCommand(RELEASE,where);
    }
    flowio.stopAction = async function(where){
      await flowio.writeCommand(STOP,where);
    }
    flowio.stopAllActions = async function(){
      await flowio.writeCommand(STOP,ALLPORTS);
    }

    flowio.setPump1PWM = async function(pwmValue){       //we will invoke this function every time the pump1 slider changes.
      flowio.pump1PWM = pwmValue; //a flag that can be read at any time if we need to know the PWM value being used.
      //send the same command as the previous one, but only change the pwmValue. Only send command if pump1 is ON.
      if(flowio.status.pump1==1){
        try{
          await flowio.writeCommand(flowio.commandArray[0],flowio.commandArray[1],pwmValue);
        }
        catch(error){
          if(error.message!="GATT operation already in progress.") flowio.log(error);
          //Display error only if different from this one. Is there a more elegant way
          //to check id device is bysy and then simplu not send the write request, rather
          //than waiting for an error to tell me this?
        }
      }
    }
    flowio.setPump2PWM = async function(pwmValue){       //we will invoke this function every time the pump1 slider changes.
      flowio.pump2PWM = pwmValue;
      //send the same command as the previous one, but only change the pwmValue. Only send command if pump1 is ON.
      if(flowio.status.pump2==1){
        try{
          await flowio.writeCommand(flowio.commandArray[0],flowio.commandArray[1],pwmValue);
        }
        catch(error){
          if(error.message!="GATT operation already in progress.") flowio.log(error);
          //Display error only if different from this one.
        }
      }
    }
    //######################################################################
    flowio.log("Control Service Initialized");
    //To get initial values for our status table, we must read the hardware status characteristic.
    flowio.getHardwareStatus(); //invoke this function to trigger our event listener.
  }
  catch(error){
    flowio.log("FlowIO initControlService() error :( " + error);
    throw "FlowIO initControlService() error :(.  ";
  }
}

//###########################--- GUI Specific Functions ---#############################
function getSelectedPorts(flowio){ //we invoke this function as an argument to the action methods, IN THE HTML!
  let portsByte = 0x00;
  if(document.querySelector(`#port1_chk${flowio.instanceNumber}`).checked) portsByte ^= 0x01; //0 0001
  if(document.querySelector(`#port2_chk${flowio.instanceNumber}`).checked) portsByte ^= 0x02; //0 0010
  if(document.querySelector(`#port3_chk${flowio.instanceNumber}`).checked) portsByte ^= 0x04; //0 0100
  if(document.querySelector(`#port4_chk${flowio.instanceNumber}`).checked) portsByte ^= 0x08; //0 1000
  if(document.querySelector(`#port5_chk${flowio.instanceNumber}`).checked) portsByte ^= 0x10; //1 0000
  return portsByte;
}

async function setPump1PWM(flowio){
  let pwmInSlider = document.getElementById(`pwm_i${flowio.instanceNumber}`);
  let pwmInLabel = document.getElementById(`pwm_i_label${flowio.instanceNumber}`);
  pwmInLabel.innerHTML = pwmInSlider.value;
  try{await flowio.setPump1PWM(pwmInSlider.value);}
  catch(error){flowio.log(error + "(>:<)");}
}
async function setPump2PWM(flowio){
  let pwmOutSlider = document.getElementById(`pwm_o${flowio.instanceNumber}`);
  let pwmOutLabel = document.getElementById(`pwm_o_label${flowio.instanceNumber}`);
  pwmOutLabel.innerHTML = pwmOutSlider.value;
  try{await flowio.setPump2PWM(pwmOutSlider.value);}
  catch(error){flowio.log(error + " (>:<)");}
}
function setSvgStatus(flowio){
  let n = flowio.instanceNumber
  document.getElementById(`svg_on_1${n}`).style.visibility = (flowio.status.port1==1)? "visible" : "hidden";
  document.getElementById(`svg_on_2${n}`).style.visibility = (flowio.status.port2==1)? "visible" : "hidden";
  document.getElementById(`svg_on_3${n}`).style.visibility = (flowio.status.port3==1)? "visible" : "hidden";
  document.getElementById(`svg_on_4${n}`).style.visibility = (flowio.status.port4==1)? "visible" : "hidden";
  document.getElementById(`svg_on_5${n}`).style.visibility = (flowio.status.port5==1)? "visible" : "hidden";
  document.getElementById(`svg_on_inlet${n}`).style.visibility = (flowio.status.inlet==1)? "visible" : "hidden";
  document.getElementById(`svg_on_outlet${n}`).style.visibility = (flowio.status.outlet==1)? "visible" : "hidden";
  document.getElementById(`svg_off_1${n}`).style.visibility = (flowio.status.port1==0)? "visible" : "hidden";
  document.getElementById(`svg_off_2${n}`).style.visibility = (flowio.status.port2==0)? "visible" : "hidden";
  document.getElementById(`svg_off_3${n}`).style.visibility = (flowio.status.port3==0)? "visible" : "hidden";
  document.getElementById(`svg_off_4${n}`).style.visibility = (flowio.status.port4==0)? "visible" : "hidden";
  document.getElementById(`svg_off_5${n}`).style.visibility = (flowio.status.port5==0)? "visible" : "hidden";
  document.getElementById(`svg_off_inlet${n}`).style.visibility = (flowio.status.inlet==0)? "visible" : "hidden";
  document.getElementById(`svg_off_outlet${n}`).style.visibility = (flowio.status.outlet==0)? "visible" : "hidden";

  document.getElementById(`svg_pump1_on${n}`).style.visibility = (flowio.status.pump1==1)? "visible" : "hidden";
  document.getElementById(`svg_pump2_on${n}`).style.visibility = (flowio.status.pump2==1)? "visible" : "hidden";
  document.getElementById(`svg_pump1_off${n}`).style.visibility = (flowio.status.pump1==0)? "visible" : "hidden";
  document.getElementById(`svg_pump2_off${n}`).style.visibility = (flowio.status.pump2==0)? "visible" : "hidden";
}
