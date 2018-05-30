const util = require('../metautil')
const laser = require('../laser')
const MetaClient = require('../client')

module.exports = {
  /**
   * Create a StreamSpec Resource.
   * @param {*} event 
   * @param {*} context 
   */
  async actionCreate(event, context, client) {
    console.log('Entered create::actionCreate(_)')

    // Create the payload we need to create the StreamDescription in Laser.
    const description = await makeLaserDescriptionMessage(event, client)

    console.log("...ABOUT TO CREATE DESCRIPTION IN LASER...")
    /*
    * Create Description in Laser
    */ 
    const laserClient = new MetaClient(laser.getAddress(event), laser.getAuth(context))
    const laserResult = await laserClient.post(`/streamDefinitions`, util.pretty(description)).then(des =>
      des.data
    ).catch(err => {
      console.error("ERROR : ")
      console.log(err)
      throw err
    })

    console.log("LASER-RESULT : " + util.pretty(laserResult))

    return injectLaserProviderInfo(event)
  }
}

async function makeLaserDescriptionMessage(event, client) {
  // This gets the stream configs from the StreamSpec payload
  const configs = {
    input: event.resource.properties.processor.inputStreamConfig,
    output: event.resource.properties.processor.outputStreamConfig
  }

  // This gets the actual DataFeed resources named in the configs from Meta
  const inputFeed = await metaGetDataFeed(configs.input.feedID, client)
  const outputFeed = await metaGetDataFeed(configs.output.feedID, client)

  console.log('INPUT-FEED:\n' + util.pretty(inputFeed))
  console.log('OUTPUT-FEED:\n' + util.pretty(outputFeed))

  // Merge data from the meta feeds and configurations into the Laser format.
  const laserInputStream = toLaserInputStream(inputFeed, configs.input)
  const laserOutputStream = toLaserOutputStream(outputFeed, configs.output)

  // Create laser StreamDescription using all the data we have.
  const streamProcessor = Object.assign(
    {type: event.resource.properties.processor.type},
    {lambdaId: event.resource.properties.processor.lambdaId},
    {inputStreamConfig: laserInputStream},
    {outputStreamConfig: laserOutputStream}
  )

  const description = Object.assign(
    {id: event.resource.id},
    {name: event.resource.name},
    {cpus: event.resource.properties.cpus},
    {mem: event.resource.properties.mem},
    {parallelization: event.resource.properties.parallelization},
    {processor: streamProcessor}
  )

  return description
}

/**
 * Merge Meta DataFeed and StreamSpec.inputStreamConfig to a Laser.inputStreamConfig 
 * @param {*} event 
 */
function toLaserInputStream(feed, config) {
  console.log('Entered toLaserInputStream()...')
  return Object.assign(
    {name: config.name},
    {type: feed.properties.kind},
    {broker: feed.properties.data.endpoint},
    {topic: feed.properties.data.topic},
    {group: feed.properties.data.group},
    {credentials: feed.properties.data.credentials},
    {partitions: config.partitions})
}

/**
 * Convert a Meta DataFeed and StreamSpec.outputStreamConfig to a Laser outputStreamConfig
 */
function toLaserOutputStream(feed, config) {
  console.log('Entered toLaserOutputStream()...')
  return Object.assign(
    {name: config.name},
    {type: feed.properties.kind},
    {broker: feed.properties.data.endpoint},
    {topic: feed.properties.data.topic},
    {credentials: feed.properties.data.credentials}
  )
}

/**
 * Get a DataFeed resource from Meta
 * @param {*} id 
 */
async function metaGetDataFeed(id, client) {
  console.log("Getting Data feed with ID : " + id)
  return client.get(`top/datafeeds/${id}`)
    .then(feed => feed.data)
    .catch(err => {
      console.error("ERROR : " + err.stack)
      throw err
    })
}

/**
 * Inject data about the Laser provider into StreamSpec.properties.
 * @param {*} event 
 */
function injectLaserProviderInfo(event) {
  const lambdaId = event.resource.properties.processor.lambdaId  
  const providerUrl = laser.getAddress(event)

  const props = event.resource.properties
  /* 
    * streamspec.properties.provider is a reference UUID in the schema. That means
    * the rendered link must be replaced with 'just' the UUID for Meta input.
    */
  const providerId = event.resource.properties.provider.id

  const nprops = Object.assign(
    Object.assign(props, { provider : providerId}), 
      { lambda_provider: { id: '12345', url: providerUrl}})

  return Object.assign(event.resource, { properties : nprops })
}

