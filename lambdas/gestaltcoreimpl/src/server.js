const http = require('http')
const concat = require('concat-stream')
const lambda = require('../index')

const server = http.createServer((request, response) => {

  if (request.method === 'POST') {
    handlePost(request, response)
  } else {
    response.end('It works! Path hit: ' + request.url)
  }
})

const PORT = process.argv.length <= 2 ? 8080 : process.argv[2]

server.listen(PORT, function() {
  console.log("server listening on : http://localhost:%s", PORT);
})



async function handlePost(request, response) {
  const chunks = [];

  request.on('data', chunk => chunks.push(chunk));
  request.on('end', () => {
    const event = JSON.parse(Buffer.concat(chunks).toString());
    const context = fakeLambdaContext(request.method, request.headers)
    
    const ccallback = (response) => 
      ((error, data) => responseCallback(response, error, data))
    
    const callback = ccallback(response)

    console.log("*******ABOUT TO CALL LAMBDA FUNCTION...")
    console.log('TYPEOF CALLBACK : ' + (typeof callback))
    
    lambda.entryPoint(JSON.stringify(event), JSON.stringify(context), callback)
  })
}

function doend(response, msg) {
  console.log("!!! Ending response without callback !!!")
  response.end()
}

function responseCallback(response, error, data) {
  console.log(">>> INSIDE RESPONSE-CALLBACK...")
  console.log(">>> RESPONSE : " + response)
  console.log(">>> ERROR : " + error)
  console.log(">>> DATA : " + data)

  var output = null
  if (error) {
    console.error("An error occurred: " + error)
    output = error
  } else {
    output = JSON.stringify(data, null, 2)
  }
  console.log(">>>OUTPUT : " + output)
  response.setHeader('Content-Type', 'application/json');
  response.end(output)
}

function fakeLambdaContext(method, headers) {
  return {
    executionId: {},
    user: uuidv4(),
    lambdaId: {},
    eventName: "rest.event.sync",
    method: method,
    headers: capHeaders(headers),
    params: {},
    responseTopic: "default-response-topic.0",
    intakeTime: 123456789009
  }
}

// from: https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
function uuidv4() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  });
}

/**
 * Capitalize the first letter in each word accounting for possible
 * compound words separated with hypens ('-' characters).  This is
 * used to translate header names from lowercase as given by NodeJS
 * to the uppercase given by Laser.
 * @param {string} s 
 */
function capHeaderName(s) {
  const capFirst = (s) => 
    s.charAt(0).toUpperCase() + s.slice(1)

  if (s.indexOf('-') < 0) {
    return capFirst(s)
  } else {
    return s.split('-').map(c => capFirst(c)).join('-')
  }
}

function capHeaders(hs) { 
  return Object.entries(hs).reduce((obj, [k, v]) => (
    Object.assign(obj, { [capHeaderName(k)]: v })
  ), {})
}