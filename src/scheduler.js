window.onload = function () {
  addRow(0); //we are creating the first row of the table dynamically on load.
};

function getScheduleTable(instanceId) {
  return document.getElementById(`schedule${instanceId}`)
}

/**
 *
 * @param instanceId
 * @return {{port5: NodeListOf<Element>, pwmVals: NodeListOf<Element>, port3: NodeListOf<Element>, actions: NodeListOf<Element>, port4: NodeListOf<Element>, port1: NodeListOf<Element>, startTimes: NodeListOf<Element>, port2: NodeListOf<Element>}}
 */
function getScheduleAsColumns(instanceId) {
  let startTimes = document.querySelectorAll(`#schedule${instanceId} .starttime`); //creates an array of all the times.
  let actions = document.querySelectorAll(`#schedule${instanceId} .action`); //creates an array of all the actions.
  let port1 = document.querySelectorAll(`#schedule${instanceId} .port1`); //creates an array of all port1 states.
  let port2 = document.querySelectorAll(`#schedule${instanceId} .port2`); //creates an array of all port2 states.
  let port3 = document.querySelectorAll(`#schedule${instanceId} .port3`); //creates an array of all port3 states.
  let port4 = document.querySelectorAll(`#schedule${instanceId} .port4`); //creates an array of all port4 states.
  let port5 = document.querySelectorAll(`#schedule${instanceId} .port5`); //creates an array of all port5 states.

  let pwmVals = document.querySelectorAll(`#schedule${instanceId} .pwm`); //creates an array of all PWM values in the the able.
  return {startTimes, actions, port1, port2, port3, port4, port5, pwmVals}
}

function areStartTimesValid(startTimes) {
  let startTimePrev = 0;
  for (let i = 0; i < startTimes.length; i++) { //index 1 is our 1st data item in the table.
    let startTime = startTimes[i].value;
    if (startTime < startTimePrev) {
      return false;
    }
    startTimePrev = startTime;
  }
  return true;
}

function executeSequence(flowio) {
  flowio.log("Executing Sequence...")
  let i = flowio.instanceNumber;
  //1. Access the schedule table.
  //2. Read the time, action, and selected ports for the first data row (which is the second
  //  row of the table, but row numbers start from 0, so it is row number "1" anyways.)
  //3. Send the parameters obtained in step 2 to the executeAction() function.
  //4. Repeat the last two steps until all rows of the table are read.
  let scheduleTable = getScheduleTable(i);
  let {startTimes, actions, port1, port2, port3, port4, port5, pwmVals} = getScheduleAsColumns(i)

  //#########---Validate if time entries are in order---############
  //This section is optional and purely for catching user entry problems.
  if (areStartTimesValid(startTimes) === false) {
    flowio.log(`Aborted - Task #${i + 1} out of order.`)
    return;
  }

  //#################---Validation Complete---######################

  for (let i = 0; i < startTimes.length; i++) {
    let p1 = port1[i].checked;
    let p2 = port2[i].checked;
    let p3 = port3[i].checked;
    let p4 = port4[i].checked;
    let p5 = port5[i].checked;

    let portsByte = getPortsByte(p1, p2, p3, p4, p5); //each input is either true or false.
    let pwmVal = pwmVals[i].value;
    let startTime = startTimes[i].value;
    let action = actions[i].value;
    if (portsByte == 0) {
      flowio.log("You did not select any ports on row " + `${i + 1}`);
    } else {
      executeAction(flowio, startTime, action, portsByte, pwmVal);
    }
  }
  flowio.log("");
}

function executeAction(flowio, time, action, portsByte, pwmVal) { //the time specifies AT WHICH TIME the action will begin, NOT the duration of the action.
  let array3byte = new Uint8Array(3);
  if (action == "+") array3byte[0] = 0x2b; //'+'
  else if (action == "-") array3byte[0] = 0x2d; //'-'
  else if (action == "^") array3byte[0] = 0x5e; //'^'
  else array3byte[0] = 0x21; //'!'

  array3byte[1] = portsByte;
  array3byte[2] = pwmVal;
  setTimeout(async function () {
    await flowio.controlService.chrCommand.writeValue(array3byte);
  }, parseInt(time));
  // setTimeout(async function(){await flowio.writeCommand(action, portsByte, pwmVal);}, parseInt(time));
}

function getPortsByte(p1, p2, p3, p4, p5) {
  let portsByte = 0x00;
  if (p1) portsByte ^= 0x01; //0 0001
  if (p2) portsByte ^= 0x02; //0 0010
  if (p3) portsByte ^= 0x04; //0 0100
  if (p4) portsByte ^= 0x08; //0 1000
  if (p5) portsByte ^= 0x10; //1 0000
  return portsByte;
}

function fromPortsByte(byte) {
  return [byte & 0x01, byte & 0x02, byte & 0x04, byte & 0x08, byte & 0x10].map(Boolean)
}

//#############---CALLBACKS---###################
function deleteRow(instanceNumber) { //delete the previous to the last row.
  let i = instanceNumber; //argument is the flowio instance number (first one is #0)
  let scheduleTable = document.getElementById(`schedule${i}`);
  let n = scheduleTable.rows.length;
  if (n > 2) scheduleTable.deleteRow(n - 2); //the first row is the title row, and is row 0.
}

/**
 * Assign values to a row in the table.
 * @param row
 * @param content Values as encoded by {@link encodeSchedule}
 */
function populateScheduleRow(row, content) {
  const {startTime, portByte, action, pwmVal} = content
  row.querySelector('.starttime').value = startTime;
  row.querySelector('.action').value = action;
  row.querySelector('.pwm').value = pwmVal;
  // If portByte is not an array, fromPortsByte will turn it to an array.
  (!Array.isArray(portByte) ? fromPortsByte(portByte) : portByte)
    .forEach((checked, portIdx) => {
    row.querySelector(`.port${portIdx + 1}`).checked = checked;
  })
}

function addRow(instanceNumber, content) {
  let i = instanceNumber; //argument is the flowio instance number (first one is #0)
  let scheduleTable = document.getElementById(`schedule${i}`);

  let n = scheduleTable.rows.length - 1; //the first row of the table is title row.
  let row = scheduleTable.insertRow(n); //I want to add the new row before the last one.
  //log(n);

  //TODO: There is a dilemma here. I need the input elements to be disabled initially, and when the initial row is created onload, I want it to be
  //disabled. But then after I connect to the device and add new row, I want that new row to not be disabled. Thus, I can put "disabled" parameter
  //to achieve (1), but then I also have it be disabled each time I add a new row which is not what I want. Conversely, if I don't put "disabled" then
  //the row created onload will not be disabled. One way to solve this is to have the first row NOT be created by the JS but hard-coded in HTML.
  //Then I can remove the onload function altogether.
  //Another solution is to check if I am connected and then have a variable ${var} be inserted, where
  //if disconnected var="disabled"
  //if connected var =""
  //Having the first row be NOT disabled is actually fine, because this is only so on initial page load. If I connect and disconnect, then it IS disabled
  //because of the x0 class. So I can just leave it like this.

  //TODO: I don't like the fact that I have to put x0 in each input field. Because that will complicate how I put schedulers in
  //my other tabs. But if I do it this way, I should then insert not x0 but x${i} where i will be passed as an argument to the addRow(i) function.
  //directly from the HTML.
  row.insertCell(0).innerHTML =
    `<input class="x${i} starttime" type="number" min="0" max="200000" value="0" step="100">`;
  row.insertCell(1).innerHTML = `
                    <select class="x${i} action">
                      <option value="+">Inflate</option>
                      <option value="-">Vacuum</option>
                      <option value="^">Release</option>
                      <option value="!">Stop</option>
                      <option value="">Open</option>
                      <option value="">Close</option>
                    </select>
                    `;
  row.insertCell(2).innerHTML = `<input class="x${i} pwm" type="number" min="100" max="255" value="255" step="5">`;
  row.insertCell(3).innerHTML = `
              <div>
                <input type="checkbox" class="x${i} port1" value="1">
                <input type="checkbox" class="x${i} port2" value="2">
                <input type="checkbox" class="x${i} port3" value="3">
                <input type="checkbox" class="x${i} port4" value="4">
                <input type="checkbox" class="x${i} port5" value="5">
              </div>
              `;
  if (content) {
    populateScheduleRow(row, content);
  }
}

function encodeSchedule({startTimes, actions, port1, port2, port3, port4, port5, pwmVals}) {
  return Object.keys(startTimes).map((_, i) => ({
    startTime: startTimes[i].value,
    portByte: [port1[i].checked, port2[i].checked, port3[i].checked, port4[i].checked, port5[i].checked],
    action: actions[i].value,
    pwmVal: pwmVals[i].value
  }))
}

/**
 * Save the current schedule as a JSON file.
 * @param flowio
 */
function saveSchedule(flowio) {
  const instanceId = flowio.instanceNumber;
  const schedule = encodeSchedule(getScheduleAsColumns(instanceId));
  const asJsonString = JSON.stringify(schedule, null, 2);
  const escaped = encodeURIComponent(asJsonString);
  // Create a <a> tag that would start download of the JSON file
  // Then click it on behalf of the user to start downloading
  const downloadElement = document.createElement('a')
  downloadElement.setAttribute('href', 'data:application/json;charset=utf-8,' + escaped);
  downloadElement.setAttribute('download', `schedule-${instanceId}.json`);
  downloadElement.setAttribute('target', '_blank');
  downloadElement.style.display = 'none';
  document.body.appendChild(downloadElement);
  downloadElement.click();
  document.body.remove(downloadElement);
}

/**
 * Read a JSON file from the '#schedule-upload' file input, validate it, and then recreate the schedule table.
 * @param flowio
 * @return {boolean}
 */
function uploadSchedule(flowio) {
  const fileElement = document.getElementById('schedule-upload');
  const file = fileElement.files[0];
  file.text()
    .then(JSON.parse)
    .then(validateSchedule)
    .then(createSchedule(flowio));
  return false;
}

function isStrictlyIncreasing(array) {
  return array.reduce(
    // An array a[0..n] is strictly increasing iff (1) a[0..n-1] is strictly increasing AND a[n] > a[n-1]
    // This fold carries over the value of a[n-1] and whether the array is strictly increasing
    ([lastValue, isIncreasing], currentValue) => [currentValue, isIncreasing && currentValue > lastValue],
    // Initial value is [Min Integer, True]: Every other integer is greater than Min Integer,
    // and an empty array is strictly increasing.
    [Number.MIN_SAFE_INTEGER, true])[1]
}

/**
 * Validate a schedule file, verifying that every row has the required fields and values are in range; and that start
 * times are strictly increasing.
 * @param json
 * @return Validated JSON
 * @throws Error if validation failed.
 */
function validateSchedule(json) {
  function validateRow(row) {
    const {startTime, pwmVal, portByte, action} = row;
    const startTimeNum = Number(startTime)
    const pwmValNum = Number(pwmVal)
    return Number.isInteger(startTimeNum) && startTimeNum >= 0
      && Number.isInteger(pwmValNum) && pwmValNum < 256 && pwmValNum >= 0
      && portByte >= 0x0 && portByte <= 0x1F
      && action != null;
  }

  if (!json.every(validateRow)) {
    throw new Error("Found an invalid row ")
  }
  if (!isStrictlyIncreasing(json.map(row => Number(row.startTime)))) {
    throw new Error("Start times must be monotonically increasing!")
  }
  return json;
}

function clearScheduleTable(flowio) {
  const table = getScheduleTable(flowio.instanceNumber);
  for (let i = 1; i < table.rows.length; i++) {
    table.deleteRow(i);
  }
}

/**
 * Curried version for re-creating the schedule table. Return a JSON receiving function that clears the table
 * and then recreate it row by row.
 * @param flowio
 * @return {function(*): void}
 */
function createSchedule(flowio) {
  return (json) => {
    clearScheduleTable(flowio)
    json.forEach(rowData => addRow(flowio.instanceNumber, rowData));
  }
}
