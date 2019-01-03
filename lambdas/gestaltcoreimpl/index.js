
const MetaClient = require('./src/client')
const meta = require('./src/functions/resources')

exports.entryPoint = function(event, context, callback) {

  const contextData = JSON.parse(context);
  const eventData = JSON.parse(event);
  const eventName = eventData.action//.toLowerCase()


  console.log('eventData.action : ' + eventData.action)

  //console.log('EVENT-DATA: ' + JSON.stringify(eventData, null, 2))
  //const metaUrl = `${process.env.META_PROTOCOL}://${process.env.META_HOSTNAME}:${process.env.META_PORT}`;
  const metaUrl = 'http://localhost:9000'
  const authorization = contextData.headers.Authorization
  const metaClient = new MetaClient(metaUrl, authorization) 

  console.log("META-URL  : " + metaUrl);

  const fn = new Map()
  fn.set('environment.create', meta.newEnvironment)


  const process = (eventName) => {
    console.log(`Received event '${eventName}'...finding handler...`)
    if (fn.has(eventName)) {
      fn.get(eventName)(eventData, contextData, null, callback)
    } else {
      callback(new Error(`Invalid event-name. found : '${eventName}'`))
    }
  }

  process(eventName)

}