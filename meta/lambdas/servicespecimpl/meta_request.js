var axios = require('axios')

const meta = 'http://localhost:9000'
const url = (resource) => `${meta}${resource}`

const client = axios.create({
  baseURL: meta,  
  headers: {
    'Authorization': 'Bearer f6f6575d-042c-4b22-af2f-d311660820ea',
    'Accept': 'application/json',
    post: {
      'Content-Type': 'application/json'
    }
  }
})

const request = (options) => {

  const Success = (response) => {
    console.debug('Request Successful')
    return response.data
  }

  const Failure = (error) => {
    console.error('Request Failed:', error.config)

    if (error.response) {
      console.error('Status: ', error.response.status)
      console.error('Body: '  , error.response.data)
      console.error('Headers: ', err.response.headers)
    } else {
      console.error('Error : ', error.message)
    }
    return Promise.reject(error.response.data || { status: 500, message: error.message })
  }

  return client(options).then(Success).catch(Failure)
}