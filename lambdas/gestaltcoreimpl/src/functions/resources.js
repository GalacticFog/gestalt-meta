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
      "id": "4efe841f-9cee-4250-8cc5-e8c85a33d2e0",
      "name": "test"
  },
  {
      "id": "96557d48-31b3-4429-86cf-49a02d84e882",
      "name": "production"
  },
  {
      "id": "79cfdd04-1c60-494d-a727-9336b5f64e6d",
      "name": "service"
  },
  {
      "id": "21a502f2-a676-466c-a3ab-cf6a78b3d49d",
      "name": "development"
  },
  {
      "id": "950056de-ad8e-4c97-b690-15a875d545cf",
      "name": "other"
  }
]

function translateEnvType(typeName, client) {
  const target = dat.filter(el => el.name === typeName)
  if (target.length === 0) {
    throw new Error(`Invalid environment_type. found: '${typeName}'`)
  } else {
    return target[0].id
  }
}