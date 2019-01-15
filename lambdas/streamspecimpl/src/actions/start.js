const util = require('../metautil')
const laser = require('../laser')
const MetaClient = require('../client')

module.exports = {

  async actionStart(event, context, client) {
    console.log('start::actionStart(_)...')

    const specId = event.resource.id
    const startUrl = `streamDefinitions/${specId}/start`

    console.log("LASER-ADDRESS : " + laser.getAddress(event))
    console.log("LASER-AUTH    : " + laser.getAuth(context))

    const laserClient = new MetaClient(laser.getAddress(event), laser.getAuth(context))
    
    console.log("Calling laser...")

    const streamInstance = await laserClient.post(startUrl, {}).then(r => {
      console.log('Stream instance STARTED: ' + util.pretty(r.data))
      return r.data
    }).catch(err => {
      console.error("ERROR : " + util.pretty(err))
      throw err
    })

    return this.updateStreamProcesses(event.resource, streamInstance)
  },

  async actionStop(event, context, client) {
    console.log('Entered start::actionStop(_)...')

    //const specId = event.resource.id
    //const startUrl = `streams/${specId}/stop`
    //const spec = event.resource

    console.log("Checking for PROCESS-ID...")
    
    let processId = null
    if (event.queryParams && event.queryParams['pid']) {
      processId = event.queryParams['pid']
    } else {
      throw new Error(`Missing query-parameter 'pid'`)
    }

    console.log('Found Process ID : ' + processId)
    const stopUrl = `streams/${processId}/stop`
    const laserClient = new MetaClient(laser.getAddress(event), laser.getAuth(context))
    const stopStatus = laserClient.post(stopUrl, {}).then(r => {
      console.log('Stream instance STOPPED: ' + util.pretty(r.data))
      return r.data
    }).catch(err => {
      console.error("ERROR : " + util.pretty(err))
      throw err
    })
    
    return this.removeStreamProcessId(event.resource, processId)
  },

  removeStreamProcessId(spec, pid) {
    console.log('start::removeStreamProcessId(_)...')

    if (!spec.properties.persistence_ids) {
      console.log('No persistence IDs found - no changes made.')
      return spec
    } else {
      console.log('Persistence IDs found - attempting to filter...')
      console.log("OLD-IDS : " + spec.properties.persistence_ids)
      const pids = spec.properties.persistence_ids.filter(id => id.toString() !== pid.toString())

      console.log("NEW-IDS : " + pids)

      return {
        ...spec,
        properties: {
          ...spec.properties,
          persistence_ids: pids
        }
      }    
    }
  },


  /**
   * Add a new, started Stream Instance to StreamSpec
   * @param {*} event 
   */
  updateStreamProcesses(spec, stream) {
    console.log('start::updateStreamProcess(_)')

    let pids = null
    if (spec.properties.persistence_ids) {
      console.log('Adding new pids array...')
      // Add new stream instance to existing array.
      const oldids = spec.properties.persistence_ids
      pids = [...oldids, stream.persistenceId]
    } else {
      console.log('Found exisiting pids...adding to array...')
      // Create array and add new stream instance.
      pids = new Array(stream.persistenceId)
    }

    return {
      ...spec,
      properties: {
        ...spec.properties,
        persistence_ids: pids
      }
    }
  }

}