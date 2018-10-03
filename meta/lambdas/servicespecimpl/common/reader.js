/**
 * Convert a ServiceSpec formatted endpoint to the Provider Type format
 * @param {*} ep 
 */
const providerEndpoint = (ep) => {
  const endpointType = ep.kind
  const actions = ep.actions
  const { username, password } = ep.authentication
  const { id } = ep.implementation
  const auth = `Basic ${new Buffer(username + ':' + password).toString('base64')}`

  const out = {
    [endpointType]: {
      authentication: auth,
      url: id
    }
  }
  return actions 
    ? Object.assign({}, out, { actions : actions }) 
    : Object.assign({}, out, { default : true })
}


/**
 * Convert a ServiceSpec.provider_def to a ProviderType resource definition.
 * @param {*} providerDef 
 */
/*
const metaProviderType = (providerDef) => {
  const {name, extend, description, endpoints} = providerDef
  return {
    name,
    extend,
    description,
    properties: {
      config: {
        endpoints: endpoints.map(ep => providerEndpoint(ep))
      }
    }
  }
}
*/

module.exports = {

metaProviderType: function (providerDef) {
  const {name, extend, description, endpoints} = providerDef
  
  return {
    name,
    extend,
    description,
    properties: providerDef.properties
    /*
    properties: {
      config: {
        endpoints: endpoints.map(ep => providerEndpoint(ep))
      }
    }
    */
  }
}

}
/*
const createTypes = (spec) => {
  const { provider_def, supported_resources } = input.properties
  const providerPayload = metaProviderType( input.properties.provider_def )

  for (r of [providerPayload, ...supported_resources]) {
    metaPost(r)
  }  
}

const metaPost = (payload) => {
  console.log("CREATING : " + payload.name)
}

const pretty = (obj) => {
  return JSON.stringify(obj, null, 2)
}

createTypes(input)
*/

