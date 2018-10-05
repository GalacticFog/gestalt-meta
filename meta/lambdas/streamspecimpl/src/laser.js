const MetaClient = require('./client')

module.exports = {
  getAddress(event) {
    const providerUrl = event.provider.properties.config.lambda_provider_url;
    if (providerUrl) {
      console.log("Found lambda_provider_url : " + providerUrl);
      return providerUrl
    } else {
      throw new Error("Missing lambda_provider_url. Could not find /properties/config/lambda_provider_url")
    }
  },
  getAuth(context) {
    return context.headers.Authorization
  }
}
