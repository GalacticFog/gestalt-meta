const axios = require('axios')
const util = require('./common/util')
const create = require('./functions/create')
const publish = require('./functions/publish')
const deploy = require('./functions/deploy')
const u = require('util')
const MetaClient = require('./common/client')
const debug = require('debug')('gestalt')


exports.entryPoint = function(event, context, callback) {

  const eventData = JSON.parse(event)
  const contextData = JSON.parse(context)
  
  util.assert(contextData.headers.Authorization, 
    'context.headers.Authorization not found. API Endpoint must be configured for authentication.')
  
  const metaUrl = util.assert(eventData.metaAddress, 'event.metaAddress URL is missing. Cannot contact Meta')
  const authorization = contextData.headers.Authorization
  const client = new MetaClient(metaUrl, authorization)
  /*
   * Handle action events.
   */ 
  const handleEvents = async () => {
    
    switch(eventData.action) {

      case 'servicespec.create':
        console.log("[handle-action]: 'servicespec.create'")
        output = create.eventCreate(eventData, contextData)
        callback(null, { status: "success", message: "nice!" })
        break

      case 'servicespec.publish':
        console.log("[handle-action]: 'servicespec.publish'")
        let out = await publish.eventPublish(eventData, contextData, client)
        console.log("OUT:\n" + JSON.stringify(out, null, 2))
        let finalResponse = publish.makePublishResponse('DUMMY_FQON', eventData.metaAddress, out)

        console.log("FINAL-RESPONSE:\n" + JSON.stringify(finalResponse, null, 2))

        callback(null, JSON.stringify({status: 'ok'}))
        /*
         * This is the updated ServiceSpec with the 'published' property inserted.
         * The current action-invoker in Meta will update the resource if we return
         * it in the callback - but then we can't send back the 'publish-response'
         * message that tells you what was created (and gives you the IDs)
         */
        //updatePublishSpec(eventData.resource, contextData.user)
        //callback(null, JSON.parse(JSON.stringify(finalResponse)))

        break
      case 'servicespec.deploy':
        console.log("[handle-action]: 'servicespec.deploy")

        try {
          let response = await deploy.eventDeploy(eventData, contextData)
          callback(null, response)
        } catch(err) {
          callback(err)
        }
        break
        
      default:
        throw new Error(
          `Unhandled action '${eventData.action}'. The provider may be out of sync with this Lambda.`
        )
    }
  }

  handleEvents()

}

