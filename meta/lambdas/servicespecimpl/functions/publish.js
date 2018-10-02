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
  eventPublish(event, context) {
    
    util.assert(event.resource.properties.provider_def, "Provider Definition is missing from ServiceSpec")
    util.assert(event.actionPayload.org_fqon, "Missing 'event.actionPayload.org_fqon'")
    
    const metaUrl = util.assert(event.metaAddress, 'event.meta URL is missing. Cannot contact Meta')
    const authorization = context.headers.Authorization
    const client = new MetaClient(metaUrl, authorization)

    const providerDefinition = reader.metaProviderType(event.resource.properties.provider_def)
    const resourceDefinitions = event.resource.properties.supported_resources
    const publishTarget = event.actionPayload.org_fqon
    
    const publishUrl = `${publishTarget}/resourcetypes`
    const testUrl = `${publishUrl}?test=true`
    console.log('*META-URL : ' + metaUrl)
    console.log('TEST-URL : ' + testUrl)
    /*
      * Test the type information we've been given against meta to rule
      * out schema/validation errors before we attempt to create them.
      */
    return this.testCreateTypes(testUrl, providerDefinition, resourceDefinitions, client)
      .then(a => {
        debug("TEST WENT GREAT - NEXT...")
        return this.createOnPublish3(publishUrl, providerDefinition, resourceDefinitions, client)
      })
      .catch(e => {
        debug("!!!!!!!!!PUBLISH POPPED!!!!!!!!!!" + e.message) 
        throw e
      })
  },
  
  /**
   * Iterates over all the types to be created for this ServiceSpec testing
   * each for schematic validity.
   * @param {*} url 
   * @param {*} provider JSON payload for Provider type
   * @param {*} resources list of JSON objects, one for each supported resource-type.
   */    
  testCreateTypes(url, provider, resources, client) {
    const fs = resources.map(r => () => client.post(url, r))

    console.log(`URL : ${url}, Provider : ${provider}`)
    console.log(`Testing Provider Type : ${provider.name} ...`)
    console.log('FS : ' + fs)

    return client.post(url, provider)
      .then(p => {
        console.log('Provider type is good.\nChecking Resource type(s)')
        return Promise.all(fs.map(f => f()))
      })
      .then(rs => {
        console.log('Resource checks passed.')
        return rs
      }).catch(e => {
        console.log('Failed provider check - ' + e.message)
      throw e 
      })  
  },
/*
  createOnPublish(url, provider, resources, client) {
    const fs = resources.map(r => () => client.post(url, r))

    console.log(`Creating Provider Type : ${provider.name}`)
    return client.post(url, provider)
      .then(p => {

        let results = fs.reduce((promise, f) =>
          promise.then(result => f().then(Array.prototype.concat.bind(result))),
          Promise.resolve([p]))

        console.log("RESULTS : " + results)
        return results
      })
  },
  */
  async createOnPublish3(url, provider, resources, client) {
    const results = await client.post(url, provider).then(p => 
      resources.map(r => client.post(url, injectProviderId(r, p)))
    )
    console.log("RESULTS : " + results)
    return results
  },




  /**
   * Update a ServiceSpec instance as 'published'
   * @param {*} url 
   * @param {*} spec 
   */
  updatePublishSpec(spec, userId) {
    const fqon = spec.org.properties.fqon
    const specUrl = `${fqon}/servicespecs/${spec.id}`
    const published = {
      publisher: userId,
      timestamp: new Date().toISOString()
    }
    const newProperties = Object.assign(spec.properties, {published: published})
    return {
      ...spec,
      properties: newProperties
    }
  },
  makePublishResponse(fqon, metaUrl, data) {
    console.log(`makePublishResponse(${fqon}, ...)`)
    const envelope = {
      status: 'SUCCESS',
      pusblished: {
        publisher: {},
        timestamp: {}
      }
    }
    const typeData = data.map(r => {
      const id = r.id 
      const href = this.getPublishUrl(fqon, metaUrl, id)
      return Object.assign({}, { id: id, name: r.name, href: href })
    })
    return Object.assign(envelope, { typesCreated: typeData })
  },
  /*
  makePublishResponse(fqon, metaUrl, data) {
    console.log(`makePublishResponse(${fqon}, ...)`)
    const envelope = {
      status: 'SUCCESS',
      pusblished: {
        publisher: {},
        timestamp: {}
      }
    }
    const typeData = data.map(r => {
      const id = r.data.id 
      const href = this.getPublishUrl(fqon, metaUrl, id)
      return Object.assign({}, { id: id, name: r.data.name, href: href })
    })
    return Object.assign(envelope, { typesCreated: typeData })
  },
*/
  getPublishUrl(fqon, metaUrl, uuid) {
    const publishUrl = `${fqon}/resourcetypes`
    return `${metaUrl}/${publishUrl}/${uuid}`
  }
};

/**
 * Add 'provider' reference property to a new resource type.
 * @param {*} resource 
 * @param {*} provider 
 */
function injectProviderId(resource, provider) {
  return {
    ...resource,
    property_defs: [
      ...resource.property_defs,
      {
        name: 'provider',
        data_type: 'resource::uuid::link',
        requirement_type: 'required',
        refers_to: provider.data.id
      }
    ]
  }
}




