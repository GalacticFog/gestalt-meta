const Minio = require('minio')
const crud = require('./src/functions/objects')

exports.entryPoint = function(event, context, callback) {

  console.log('-------------------ENTRY-POINT--------------------')
  
  const contextData = JSON.parse(context)
  const eventData = JSON.parse(event)
  const eventName = eventData.action
  const provider = eventData.provider
  const config = provider.properties.config

  const minioClient = new Minio.Client({
      endPoint: config.address,
      port: config.port,
      secure: false,
      accessKey: config.key,
      secretKey: config.secret
  }); 

  const fn = new Map()
  fn.set('provider.bucketCreate', crud.bucketCreate)
  fn.set('provider.bucketDelete', crud.bucketDelete)
  fn.set('provider.bucketsList', crud.bucketsList)
  fn.set('provider.bucketExists', crud.bucketExists)

  const process = (eventName) => {
    console.log(`Received event '${eventName}'...finding handler...`)
    if (fn.has(eventName)) {
      console.log("Calling '${eventName}'")
      fn.get(eventName)(eventData, contextData, minioClient, callback)
    } else {
      callback(new Error(`Invalid event-name. found : '${eventName}'`))
    }
  }

  process(eventName)

}

