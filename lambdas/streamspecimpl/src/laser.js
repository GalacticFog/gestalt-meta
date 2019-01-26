const MetaClient = require('./client')

module.exports = {
  getAddress(event) {

    if (event.provider.properties.config.lambda_provider_url) {
      const lambdaUrl = event.provider.properties.config.lambda_provider_url
      console.log("Found existing lambda_provider.url: " + lambdaUrl)
      return lambdaUrl 
    } else {
      throw new Error('Could not find Lambda Provider URL.')
    }
  },
  getAuth(context) {
    //return "Bearer e7a861bc-33da-44f9-80c3-1ec69b17f724"
    console.log("Returning Auth : " + context.headers.Authorization)
    return context.headers.Authorization
  }
}