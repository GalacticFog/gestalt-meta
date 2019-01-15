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
    //return "Bearer e7a861bc-33da-44f9-80c3-1ec69b17f724"
    console.log("Returning Auth : " + context.headers.Authorization)
    return context.headers.Authorization
  }
}