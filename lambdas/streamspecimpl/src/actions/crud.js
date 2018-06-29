const util = require('../metautil')
const laser = require('../laser')
const MetaClient = require('../client')

module.exports = {
  async actionCreate(event, context, client) {
    console.log('Entered create::actionCreate(_)')
    const description = await makeLaserDescriptionMessage(event, client);

    console.log("...ABOUT TO CREATE StreamProcessDefinition IN LASER...");
    const laserClient = new MetaClient(laser.getAddress(event), laser.getAuth(context));
    const laserResult = await laserClient.post(
      `/streamDefinitions`, util.pretty(description)).then(des =>
        des.data
    ).catch(err => {
      console.error("ERROR : " + err.stack);
      throw err
    })
    console.log("LASER-RESULT : " + util.pretty(laserResult));

    return injectLaserProviderInfo(event);
  },

  async actionUpdate(event, context, client) {
    console.log('Entered create::actionUpdate(_)');
    const description = await makeLaserDescriptionMessage(event, client);

    console.log("...ABOUT TO UPDATE StreamProcessDefinition IN LASER...");
    const laserClient = new MetaClient(laser.getAddress(event), laser.getAuth(context));
    const laserResult = await laserClient.put(
      `/streamDefinitions/${event.resource.id}`, util.pretty(description)
    ).then(des => des.data).catch(err => {
      console.error("ERROR : " + err.stack);
      throw err
    })
    console.log("LASER-RESULT : " + util.pretty(laserResult));

    return injectLaserProviderInfo(event);
  },

  async actionDelete(event, context, client) {
    console.log('Entered create::actionDelete(_)');

    console.log("...ABOUT TO DELETE StreamProcessDefinition IN LASER...");
    const laserClient = new MetaClient(laser.getAddress(event), laser.getAuth(context));
    const laserResult = await laserClient.delete(
      `/streamDefinitions/${event.resource.id}`
    ).then(des => des.data).catch(err => {
      console.error("ERROR : " + err.stack);
      throw err
    })
    console.log("LASER-RESULT : " + util.pretty(laserResult));

    return {};
  }
}

async function makeLaserDescriptionMessage(event, client) {
  // This gets the stream configs from the StreamSpec payload
  const configs = {
    input: event.resource.properties.processor.inputStreamConfig,
    output: event.resource.properties.processor.outputStreamConfig
  };

  // This gets the actual DataFeed resources named in the configs from Meta
  const inputFeed = await metaGetDataFeed(configs.input.feedID, client);
  const outputFeed = await metaGetDataFeed(configs.output.feedID, client);

  let credSecrets = [];

  let inputCreds = inputFeed.credentials;
  if (inputCreds && inputCreds.secret_id && inputCreds.secret_key) {
      inputCreds = null;
      inputCredsSecret = metaGetSecret(inputCreds.secret_id, client);
      console.log(`mounting input secret ${outputCredsSecret.id}`)
      // credSecrets.push({
      //     secret_id: inputCredsSecret.id,
      //     path: '/tmp/input-creds.jks',
      //     secret_key: inputCreds.secret_key
      // });
  };

  let outputCreds = outputFeed.credentials;
  if (outputCreds && outputCreds.secret_id) {
      outputCreds = null;
      outputCredsSecret = metaGetSecret(outputCreds.secret_id, client);
      console.log(`mounting output secret ${outputCredsSecret.id}`)
      // credSecrets.push({
      //     secret_id: outputCredsSecret.id,
      //     path: '/tmp/output-creds.jks',
      //     secret_key: outputCreds.secret_key
      // });
  }

  console.log('INPUT-FEED:\n' + util.pretty(inputFeed))
  console.log('OUTPUT-FEED:\n' + util.pretty(outputFeed))

  // Merge data from the meta feeds and configurations into the Laser format.
  const laserInputStream = toLaserInputStream(inputFeed, configs.input, null);
  const laserOutputStream = toLaserOutputStream(outputFeed, configs.output, null);

  return {
      id: event.resource.id,
      name: event.resource.name,
      cpus: event.resource.properties.cpus,
      mem: event.resource.properties.mem,
      parallelization: event.resource.properties.parallelization,
      processor: {
          type: event.resource.properties.processor.type,
          lambdaId: event.resource.properties.processor.lambdaId,
          inputStreamConfig: laserInputStream,
          outputStreamConfig: laserOutputStream
      },
      secrets: credSecrets
  }
}

/**
 * Merge Meta DataFeed and StreamSpec.inputStreamConfig to a Laser.inputStreamConfig 
 * @param {*} event 
 */
function toLaserInputStream(feed, config, credentials) {
  console.log('Entered toLaserInputStream()...')
  return {
    type: feed.properties.kind,
    broker: feed.properties.data.endpoint,
    topic: feed.properties.data.topic,
    group: feed.properties.data.group,
    credentials: credentials,
    partitions: config.partitions
  }
}

/**
 * Convert a Meta DataFeed and StreamSpec.outputStreamConfig to a Laser outputStreamConfig
 */
function toLaserOutputStream(feed, config, credentials) {
  console.log('Entered toLaserOutputStream()...')
  return {
      type: feed.properties.kind,
      broker: feed.properties.data.endpoint,
      topic: feed.properties.data.topic,
      credentials: credentials
  }
}

/**
 * Get a DataFeed resource from Meta
 * @param {*} id 
 */
async function metaGetDataFeed(id, client) {
    console.log("Getting Data Feed with ID : " + id)
    return client.get(`top/datafeeds/${id}`)
        .then(feed => feed.data)
        .catch(err => {
            console.error("ERROR : " + err.stack);
            throw err
        })
}

/**
 * Get a Secret resource from Meta
 * @param {*} id
 */
async function metaGetSecret(id, client) {
    console.log("Getting Secret with ID : " + id);
    return client.get(`top/secrets/${id}`)
        .then(secret => secret.data)
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
  const props = event.resource.properties
  /* 
    * streamspec.properties.provider is a reference UUID in the schema. That means
    * the rendered link must be replaced with 'just' the UUID for Meta input.
    */
  const providerId = event.resource.properties.provider.id

  const nprops = Object.assign(props, { provider : providerId})

  return Object.assign(event.resource, { properties : nprops })
}

