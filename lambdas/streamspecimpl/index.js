const start = require('./src/actions/start')
const create = require('./src/actions/create')
const util = require('./src/metautil')
const MetaClient = require('./src/client')

exports.entryPoint = function(event, context, callback) {

  const contextData = JSON.parse(context)
  const eventData = JSON.parse(event)
  const metaUrl = eventData.metaAddress
  const authorization = contextData.headers.Authorization
  const metaClient = new MetaClient(metaUrl, authorization) 
  
  console.log("META-URL  : " + metaUrl)
  console.log("META-AUTHORIZATION : " + authorization)
  console.log('Query-Params : ' + util.pretty(eventData.queryParams))

  const handleEvents = async () => {
    
    switch(eventData.action) {
      case 'streamspec.create':
        console.log('Received [streamspec.create]')
        
        const result = await create.actionCreate(eventData, contextData, metaClient)
        callback(null, result)

        break
      case 'streamspec.start':
        console.log('Received [streamspec.start]')

        const started = await start.actionStart(eventData, contextData, metaClient)
        const sendback = translateProviderLink(started)

        console.log('RETURN-VALUE : ' + util.pretty(sendback))

        callback(null, sendback)

        break
      case 'streamspec.stop':
        console.log('Received [streamspec.stop]')

        const stopped = await start.actionStop(eventData, contextData, metaClient)
        callback(null, translateProviderLink(stopped))

        break
        case 'streamspec.view':
        console.log('STREAMSPEC.VIEW')
        break
      case 'streamspec.update':
        console.log('STREAMSPEC.UPDATE')
        break
      case 'streamspec.delete':
        console.log('STREAMSPEC.DELETE')
        break        
      default:
        console.log(`ERROR : Unknown event - ${eventData.action}`)  
        throw new Error(
          `Unhandled action '${eventData.action}'. The provider may be out of sync with this Lambda.`)   
    }
  }

  const result = handleEvents()

  function translateProviderLink(spec) {
    const props = spec.properties;
    const providerId = {
      if (props.provider.id !== undefined) {
        return props.provider.id
      } else {
        return props.provider
      }
    }
    const nprops = Object.assign(Object.assign(props, { provider : providerId}))
    console.log('translateProviderLink => output properties : ' + util.pretty(nprops))
    return Object.assign(spec, { properties : nprops })    
  }
  
}









