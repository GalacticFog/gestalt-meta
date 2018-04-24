const MetaClient = require('./client')

module.exports = {
  getAddress(event) {
    
    if (event.resource.properties.lambda_provider) {
      console.log("Found existing lambda_provider.url")
      return event.resource.properties.lambda_provider.url
    } else {
      return 'http://laser.test.galacticfog.com'
    }
  },
  getAuth(context) {
    //return "Bearer 8bd51de3-5f26-4d26-bc15-7ba7bb233da1"
    return context.headers.Authorization
  }
}