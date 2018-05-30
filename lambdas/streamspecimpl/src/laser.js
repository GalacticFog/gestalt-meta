const MetaClient = require('./client')

module.exports = {
  getAddress(event) {
    if (event.resource.properties.lambda_provider) {
      return event.resource.properties.lambda_provider.url
    } else {
      return 'http://laser.test.galacticfog.com'
    }
  },
  getAuth(context) {
    return "Bearer ba59d0da-aca5-4d1b-856c-6f65730e6676"
  }
}