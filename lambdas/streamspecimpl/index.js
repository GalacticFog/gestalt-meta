
const MetaClient = require('./src/client')

const start = require('./src/actions/start')
const create = require('./src/actions/create')
const util = require('./src/metautil')

exports.entryPoint = function(event, context, callback) {

  const contextData = context //JSON.parse(context)
  const eventData = event //JSON.parse(event)

  // const laserUrl = getLaserAddress(eventData)  
  // const laserauth = "Bearer 2d150bd3-d710-47ef-990f-88b43a41a4d1"

  const metaUrl = eventData.metaAddress
  const authorization = contextData.headers.Authorization
  const metaClient = new MetaClient(metaUrl, authorization) 
  
  console.log("META-URL  : " + metaUrl)
  console.log("META-AUTHORIZATION : " + authorization)
  console.log("ORIGINAL-URL : " + eventData.requestUrl)
  console.log('Query-Params : ' + util.pretty(eventData.queryParams))

  //console.log("QUERY-PARAMS : " + )

  const handleEvents = async () => {
    
    switch(eventData.action) {
      case 'streamspec.create':
        console.log('STREAMSPEC.CREATE')
        
        const result = await create.actionCreate(eventData, contextData, metaClient)
        callback(null, result)

        break
      case 'streamspec.start':
        console.log('STREAMSPEC.START')

        const started = await start.actionStart(event, context, metaClient)
        const sendback = translateProviderLink(started)
        callback(null, sendback)

        break
      case 'streamspec.stop':
        console.log('STREAMSPEC.STOP')

        const stopped = await start.actionStop(event)
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
    const props = spec.properties
    const providerId = props.provider.id
    
    const nprops = Object.assign(Object.assign(props, { provider : providerId}))

    return Object.assign(spec, { properties : nprops })    
  }
  
}