const axios = require('axios')
const MetaClient = require('../common/client')

const reader = require('../common/reader')
const util = require('../common/util')
const debug = require('debug')('gestalt')

const u = require('util')

module.exports = {

  /**
   * 
   * @param {*} event 
   * @param {*} context 
   */
  async eventDeploy(event, context) {
    const metaUrl = util.assert(event.metaAddress, 'event.meta URL is missing. Cannot contact Meta')
    const authorization = context.headers.Authorization
    const client = new MetaClient(metaUrl, authorization)

    const location = event.actionPayload.location_uri
    const provider = event.resource.properties.provider_def

    try {
      const serviceEndpoints = await createImplementation(metaUrl, location, provider, client)
      
      console.log('Endpoints created...')
      console.log('Creating Service Provider...')
      console.log(JSON.stringify("*****SECOND-EPS : " + serviceEndpoints, null, 2))

      const definition = event.actionPayload.definition
      const serviceProvider = await createServiceProvider(metaUrl, location, definition, serviceEndpoints, client)
    
      console.log('Service-Provider created...')
    } catch(err) {
      console.error("ERROR : " + err.message)
      
    }
    return JSON.stringify(serviceProvider, null, 2)
    return JSON.stringify({ status: 'OK', message: 'Testing'}, null, 2)
  }
};

async function createImplementation(metaUrl, location, provider, client) {

  //
  // TODO: Move all these conditionals to a validation function.
  //

  console.log(JSON.stringify(provider, null, 2))

  if (!provider.api) {
    // There is no implementation given
    throw new Error("'provider_def.api' not found.")
  }

  const api = provider.api

  if (!api.implementation) {
    throw new Error("'provider_def.api.implementation' not found.")
  }

  const impl = api.implementation

  if (!impl.kind === "Lamabda") {
    throw new Error("Unsupported 'provider_def.api.implementation.kind'. found: " + ep.implementation.kind)
  }
  
  if (!impl.spec) {
    throw new Error("Must provide data for 'provider_def.api.implementation.spec'")
  }

  const spec = impl.spec

  if (!api.endpoints) {
    throw new Error("'provider_def.api.endpoints' not found.")
  }

  //
  // Create API
  //
  const apiData = {
    name: api.name,
    description: api.description,
    kong: api.kong_provider
  }
  const metaApi = await createApi(metaUrl, location, apiData, client)
  const metaApiId = metaApi.id

  //
  // Create Lambda
  //
  const metaLambda = await createLambda(metaUrl, location, spec, client)
  const metaLambdaId = metaLambda.id

  //
  // Create Endpoints
  //
  const endpointData = {
    apiId: metaApiId,
    lambdaId: metaLambdaId
  }


  /*
  const eps = api.endpoints.map(async ep => {
    console.log('Creating Endpoint : ' + ep.path)
    const metaEndpoint = 
      await createApiEndpoint(metaUrl, location, Object.assign(endpointData, { path: ep.path }), client)
        .then(p => {
          // TODO: use metaEndpoint.properties.public_url!!!
        //console.log('P : ' + JSON.stringify(p, null, 2))

          const providerEndpoint = providerEndpointConfig(ep, p.properties.public_url)
          console.log("*****PUBLIC-URL : " + p.properties.public_url)
          console.log("PROVIDER-ENDPOINT: " + JSON.stringify(providerEndpoint, null, 2))
          return providerEndpoint
        })

    const pEndpoint = providerEndpointConfig(ep, metaEndpoint)
    console.log("META-ENDPOINT : " + JSON.stringify(metaEndpoint, null, 2))
    
    return metaEndpoint

  })
  */

  const eps = api.endpoints.map(async ep => {
    await createEnd(ep, endpointData, metaUrl, location, client)
  })


  console.log("*****FIRST-EPS : " + eps)

  return eps //Promise.all(eps)
}

async function createEnd(ep, endpointData, metaUrl, location, client) {
  console.log('Creating Endpoint : ' + ep.path)
  const pe = await createApiEndpoint(metaUrl, location, Object.assign(endpointData, { path: ep.path }), client)
    .then(p => {
      const providerEndpoint = providerEndpointConfig(ep, p.properties.public_url)
      console.log("PROVIDER-ENDPOINT: " + JSON.stringify(providerEndpoint, null, 2))
      return providerEndpoint
    })
  return pe

}

function providerEndpointConfig(specEndpoint, publicUrl) {
  const providerEndpoint = {
    ...specEndpoint,
    http: {
      url: publicUrl
    }
  }
  delete providerEndpoint.kind
  delete providerEndpoint.path
  return providerEndpoint
}

async function createApi(meta, location, data, client) {
  console.log("API-DATA : " + JSON.stringify(data, null, 2))

  const gateway = await fetchGatewayProvider(meta, location, client)
  const payload = {
    name: data.name,
    description: data.description,
    properties: {
      provider: {
        id: gateway.id,
        locations: [data.kong]
      }
    }
  }
  const url = `${meta}/${trimLeading(location)}/apis`
  const metaApi = await client.post(url, payload).then(p => {
    console.log('API-CREATED : ' + p.data.id)
    return p.data
  }).catch(err => {
    console.error('ERROR : ' + err.message)
    throw err
  })
  return metaApi
}

function parseFqon(location) {
  return trimLeading(location).split('/')[0]
}

async function createApiEndpoint(meta, location, data, client) {
  console.log('APIENDPOINT-DATA : ' + JSON.stringify(data, null, 2))
  //const url = `${meta}/${trimLeading(location)}/apis/${data.apiId}/apiendpoints`
  const url = `${meta}/${parseFqon(location)}/apis/${data.apiId}/apiendpoints`
  const payload = {
    name: data.path,
    description: "",
    properties: {
      resource: data.path,
      methods: ["POST"],
      plugins: {
        rateLimit: {
          enabled: false,
          perMinute: 60
        },
        gestaltSecurity: {
          enabled: true,
          users: [],
          groups: []
        }
      },
      synchronous: true,
      implementation_id: data.lambdaId,
      implementation_type: "lambda"
    }
  }

  console.log('POSTING : ' + url)
  console.log(JSON.stringify(payload, null, 2))
  const metaEndpoint = await client.post(url, payload).then(ep => {
    console.log('ENDPOINT CREATED : ' + ep.data.id)
    return ep.data
  })

  const tmpEndpoint = await client.get(url + "/" + metaEndpoint.id).then( p => {
    console.log('GOT ENDPOINT FROM META : ' + p.data.id)
    console.log('and public-url : ' + p.data.properties.public_url)
    return p.data
  })
  /*
   * Need endpoint.properties.public_url from return value.
   */
  return tmpEndpoint
}

async function createLambda(meta, location, spec, client) {
  const url = `${meta}/${trimLeading(location)}/lambdas`
  console.log('POST ' + url)

  const metaLambda = await client.post(url, spec).then(lam => {
    console.log('Created Lambda:\n' + JSON.stringify(lam.data.id, null, 2))
    return lam.data
  })
  return metaLambda
}

async function createServiceProvider(meta, location, providerDefinition, serviceEndpoints, client) {
  
  console.log("*****THIRD-EPS : " + serviceEndpoints)
  console.log("*****INSPECT : " + u.inspect(serviceEndpoints))

  const url = `${meta}/${trimLeading(location)}/providers`
  const providerPayload = {
    ...providerDefinition,
    properties: {
      ...providerDefinition.properties,
      config: {
        endpoints: serviceEndpoints
      }
    }
  }
  
  console.log('ENDPOINT FOR SERVICE-PROVIDER')
  console.log("service-endpoint: " + serviceEndpoints)
  console.log('service-endpoint: ' + JSON.stringify(serviceEndpoints, null, 2))
  console.log('POST ' + url)
  console.log(JSON.stringify(providerPayload, null, 2))
  
  const sp = await client.post(url, providerPayload)
    .then(p => {
      console.log('Provider created in Meta: ' + p.data.id)
      return p.data
    })
    .catch(e => {
      console.error('ERROR CREATING SERVICE-PROVIDER: ' + e.message)
      throw e
    })
  return sp
}

function trimLeading(str) {
  return str.replace(/^\/+/, '')
}

async function fetchGatewayProvider(meta, location, metaClient) {
  const url = `${meta}/${trimLeading(location)}/providers?expand=true&type=GatewayManager`
  const ps = await metaClient.get(url).then(providers => providers.data)
  
  if (ps.length != 1) throw Error('Could not get Gateway Manager provider');
  return ps[0]
}

/*
651310cd-92b7-4cc6-bf89-1ed0f66adcfd

https://meta.test.galacticfog.com/engineering/apiendpoints/651310cd-92b7-4cc6-bf89-1ed0f66adcfd?force=true

function createImplementation2(metaUrl, location, provider) {
  if (!provider.endpoints) {
    // There is no implementation given
    return { message: 'No endpoints found.'}
  }

  const eps = provider.endpoints.map(ep => {
    console.log("Found endpoints...")
    console.log('kind : ' + ep.kind)
    console.log('actions : ' + JSON.stringify(ep.actions))
    console.log('implementation-type : ' + ep.implementation.kind)

    if (!ep.implementation.kind === 'Lambda') {
      throw new Error('Unsupported endpoint.implementation.kind. found: ' + ep.implementation.kind)
    }

    if (!ep.implementation.spec) {
      throw new Error('Must provide data for endpoint.implementation.spec')
    }

    const lambdaSpec = ep.implementation.spec
    const url = `${metaUrl}/${trimLeading(location)}/lambdas`

    console.log('***POST ' + url)
    console.log(JSON.stringify(lambdaSpec, null, 2))

    ep
  })

  return eps
}
Object: Fidget
  actions: spin, stop


api: {
  "kind": "http",
  "name": "fidget_impl_v1",
  "implementation": {
    "kind": "lambda",
    "spec": {

    }
  },
  "endpoints": [
    {
      "path": "/invoke",
      "actions": ["fidget.spin", "fidget.stop"]
    }
  ]
}

- Create API
- [foreach endpoint]:
  - if there is an 'implementation.spec', create lambda
  - create endpoint

How do i know which lambda to create a given endpoint against???



  what if you want to use multiple endpoints, but you only create one lambda inline?

***** CREATE API *****

POST .../environments/{id}/apis
{
  "name": "resource-impl-1",
  "description": "API implementing functions for a resource.",
  "properties": {
    "provider": {
      "locations": [
        "b9cb99d4-32c0-495b-8bcb-680ba3d6b80e"
      ],
      "id": "644a2ea6-8648-46cc-b69a-db9023968e57"
    }
  }
}

***** CREATE ENDPOINT *****

POST .../apis/{id}/apiendpoints
{
  "name": "/invoke",
  "description": "",
  "properties": {
    "resource": "/invoke",
    "methods": [
      "GET",
      "POST"
    ],
    "plugins": {
      "rateLimit": {
        "enabled": false,
        "perMinute": 60
      },
      "gestaltSecurity": {
        "enabled": true,
        "users": [],
        "groups": []
      }
    },
    "synchronous": true,
    "implementation_id": "a7f499ff-6428-4a45-9e2c-eac32860d8fb",
    "implementation_type": "lambda"
  }
}

After create endpoint - take 'properties.public_url' for provider endpoint.

{
  "kind": "http",
  "actions": [
    "fidget.spin",
    "fidget.stop"
  ],
  "path": "/invoke"
}

{
  "actions":[
    "servicespec.publish",
    "servicespec.deploy", 
    "servicespec.import", 
    "servicespec.export"
  ],
  "http":{
    "url":"https://gtw1.test.galacticfog.com/sy-dev-api/secure1"
  }
}          

POSTING : https://meta.test.galacticfog.com/engineering/environments/58b13c79-ef24-4ee8-8f5a-b081f2e3fc63/apis/ae96b6a4-ee6c-4f61-8f31-245b6bf519ea/apiendpoints
{
  name: "/invoke",
  description: "",
  properties: {
    resource: "/invoke",
    methods: ["POST"],
    plugins: {
      rateLimit: {
        enabled: false,
        perMinute: 60
      },
      gestaltSecurity: {
        enabled: true,
        users: [],
        groups: []
      }
    },
    synchronous: true,
    implementation_id: "5cf8323d-e181-46e3-85cc-898feaee2eae",
    implementation_type: "lambda"
  }
}

*/