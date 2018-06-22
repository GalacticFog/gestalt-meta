const start = require('./src/actions/start')
const create = require('./src/actions/create')
const util = require('./src/metautil')
const MetaClient = require('./src/client')

exports.entryPoint = function(event, context, callback) {

  const contextData = JSON.parse(context)
  const eventData = JSON.parse(event)
  const metaUrl = `${process.env.META_PROTOCOL}://${process.env.META_HOSTNAME}:${process.env.META_PORT}`
  const authorization = contextData.headers.Authorization
  const metaClient = new MetaClient(metaUrl, authorization) 
  
  console.log("META-URL  : " + metaUrl)
  console.log('Query-Params : ' + util.pretty(eventData.queryParams))

  const handleEvents = async () => {
    
    switch(eventData.action) {
      case 'streamspec.create':
        console.log('Received [streamspec.create]')
        
        const result = await create.actionCreate(eventData, contextData, metaClient)
        callback(null, util.pretty(result))

        break
      case 'streamspec.start':
        console.log('Received [streamspec.start]')

        const started = await start.actionStart(eventData, contextData, metaClient)
        const response = translateProviderLink(started)

        callback(null, util.pretty(response))

        break
      case 'streamspec.stop':
        console.log('Received [streamspec.stop]')

        const stopped = await start.actionStop(eventData, contextData, metaClient)
        const response = translateProviderLink(stopped)
        callback(null, util.pretty(response))

        break
        case 'streamspec.view':
        console.log('STREAMSPEC.VIEW')
        break
      case 'streamspec.update':
        console.log('STREAMSPEC.UPDATE')
        callback("streamspec.update not supported", "streamspec.update not supported")
        break
      case 'streamspec.delete':
        console.log('STREAMSPEC.DELETE')
        callback("streamspec.delete not supported", "streamspec.delete not supported")
        break
      default:
        console.log(`ERROR : Unknown event - ${eventData.action}`)  
        throw new Error(
          `Unhandled action '${eventData.action}'. The provider may be out of sync with this Lambda.`)   
    }
  }

  const result = handleEvents()

  function translateProviderLink(spec) {
    const props = spec.properties
    const providerId = props.provider.id
    const newProps = Object.assign(props, {provider : providerId})
    return Object.assign(spec, {properties : newProps})
  }
  
}









