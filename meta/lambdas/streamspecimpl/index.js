const crud = require('./src/actions/crud')
const actions = require('./src/actions/actions')
const util = require('./src/metautil')
const MetaClient = require('./src/client')

exports.entryPoint = function(event, context, callback) {

  const contextData = JSON.parse(context);
  const eventData = JSON.parse(event);
  const metaUrl = `${process.env.META_PROTOCOL}://${process.env.META_HOSTNAME}:${process.env.META_PORT}`;
  const authorization = contextData.headers.Authorization
  const metaClient = new MetaClient(metaUrl, authorization) 
  
  console.log("META-URL  : " + metaUrl);

  const handleEvents = async () => {
    
    switch(eventData.action) {
      case 'streamspec.create':
        console.log('Received [streamspec.create]');
        const result = await crud.actionCreate(eventData, contextData, metaClient);
        callback(null, util.pretty(result));
        break;

      case 'streamspec.start':
        console.log('Received [streamspec.start]');
        const started = await actions.actionStart(eventData, contextData, metaClient);
        callback(null, util.pretty(translateProviderLink(started)));
        break;

      case 'streamspec.stop':
        console.log('Received [streamspec.stop]');
        const stopped = await actions.actionStop(eventData, contextData, metaClient);
        callback(null, util.pretty(translateProviderLink(stopped)));
        break;

      case 'streamspec.view':
        console.log('STREAMSPEC.VIEW');
        break;

      case 'streamspec.update':
        console.log('STREAMSPEC.UPDATE');
        const updated = await crud.actionUpdate(eventData, contextData, metaClient);
        callback(null, util.pretty(updated));
        break;

      case 'streamspec.delete':
        console.log('STREAMSPEC.DELETE');
        const deleted = await crud.actionDelete(eventData, contextData, metaClient);
        callback(null, '{}');
        break;

      default:
        console.log(`ERROR : Unknown event - ${eventData.action}`);
        throw new Error(
          `Unhandled action '${eventData.action}'. The provider may be out of sync with this Lambda.`
        )
    }
  }

  const result = handleEvents();

  function translateProviderLink(spec) {
    const props = spec.properties;
    const providerId = props.provider.id;
    const newProps = Object.assign(props, {provider : providerId});
    return Object.assign(spec, {properties : newProps});
  }
  
}
