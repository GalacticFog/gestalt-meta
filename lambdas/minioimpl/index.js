const crud = require('./src/functions/objects')
//const actions = require('./src/actions/actions')
//const util = require('./src/metautil')

exports.entryPoint = function(event, context, callback) {

  const contextData = JSON.parse(context);
  const eventData = JSON.parse(event);
  const provider = eventData.provider
  const config = provider.properties.config
  const minioClient = new Minio.Client({
      endPoint: config.address,
      port: config.port,
      secure: false,
      accessKey: config.key,
      secretKey: config.secret
  }); 
  
  const handleEvents = async () => {
    
    switch(eventData.action) {
      case 'provider.createBucket':
        console.log('Received [provider.createBucket]');
        const result = await crud.createBucket(eventData, contextData, minioClient, callback);
        
        callback(null, JSON.parse(result, null, 2));

        break;

      default:
        console.log(`ERROR : Unknown event - ${eventData.action}`);
        throw new Error(
          `Unhandled action '${eventData.action}'. The provider may be out of sync with this Lambda.`
        )
    }
  }

  const result = handleEvents();

}
