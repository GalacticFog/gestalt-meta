var axios = require('axios')

module.exports = class MetaClient {

  constructor(baseUrl, auth) {
    this.client = axios.create({
      baseURL: baseUrl,  
      headers: {
        'Authorization': auth,
        'Accept': 'application/json',
        post: {
          'Content-Type': 'application/json'
        },
        put: {
          'Content-Type': 'application/json'
        }
      }
    })
  }

  request(options) {
  
    const Success = (response) => {
      console.debug('Request Successful', response.status)
      return Promise.resolve({ code: response.status, data: response.data })
    }
  
    const Failure = (error) => {
      console.error('Error : ', error.message)
      return Promise.reject(
        error.response ? error.response.data : error.message || { code: 500, data: error.message }
      )
    }
  
    return this.client(options).then(Success).catch(Failure)
  }

  get(uri) {
    console.log(`Executing GET ${uri}`);
    return this.request({
      method: 'GET',
      url: uri
    })
  }

  put(uri, payload) {
    console.log(`Executing PUT ${uri}`);
    return this.request({
      method: 'PUT',
      data: payload,
      url: uri
    })
  }

  post(uri, payload) {
    console.log(`Executing POST ${uri}`);
    return this.request({
      method: 'POST',
      data: payload,
      url: uri
    })
  }
  
  postEmpty(uri) {
    console.log(`Executing POST ${uri}`);
    return this.request({
      method: 'POST',
      url: uri
    })
  }

  patch(uri, payload) {
    console.log(`Executing PATCH ${uri}`);
    return this.request({
      method: 'PATCH',
      data: payload,
      url: uri
    })
  }

  delete(uri, qs) {
    console.log(`Executing DELETE ${uri}`);
    return this.request({
      method: 'DELETE',
      params: qs,
      url: uri,
      validateStatus: function (status) {
        return (status >= 200 && status < 300) || (status == 404);
      }
    })
  }
}
