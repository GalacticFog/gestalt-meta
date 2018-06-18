const MetaClient = require('./client')

module.exports = {
  getAddress(event) {
    
    const providerUrl = event.provider.properties.config.lambda_provider_url
    if (providerUrl) {
      console.log("Found lambda_provider_url : " + providerUrl)
      return providerUrl
    } else {
      throw new Error("Missing lambda_provider_url. Could not find /properties/config/lambda_provider_url")
    }
  },
  getAuth(context) {
    //return "Bearer e7a861bc-33da-44f9-80c3-1ec69b17f724"
    return context.headers.Authorization
  }
}
