module.exports = {

  /**
   * Transform the environment payload - check for storage specification
   * and create if necessary
   */
  async newEnvironment(event, context, client, cb) {
    // Translate environment type from string to UUID
    const envTypeId = translateEnvType("production")

    const updated = {
      ...event.resource,
      properties: {
        ...event.resource.properties,
        environment_type: envTypeId
      }
    }

    cb(null, updated)
  }
}

const dat = [
  {
      "id": "81328db9-a773-426f-b857-ddb45a152d47",
      "name": "test"
  },
  {
      "id": "248088cd-8b84-4190-9af9-443a91efffff",
      "name": "production"
  },
  {
      "id": "46b0688f-0469-4872-b6ac-0731c8c7f734",
      "name": "service"
  },
  {
      "id": "904b7c60-a716-45d1-8735-894eb9cc82b5",
      "name": "development"
  },
  {
      "id": "cbfe8f91-ede3-444e-9a75-9cb2bb4031b6",
      "name": "other"
  }
]
/*
    	"caas_provider": {
    		"id": "{{kube-provider-1}}"
      },
      */
function translateEnvType(typeName, client) {
  const target = dat.filter(el => el.name === typeName)
  if (target.length === 0) {
    throw new Error(`Invalid environment_type. found: '${typeName}'`)
  } else {
    return target[0].id
  }
}