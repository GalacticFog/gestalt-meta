import request from './index.js'

const get = (uri) => {
  return request({
    url: uri,
    method: 'GET'
  })
}

const MetaResource = {
  get
}

export default MetaResource