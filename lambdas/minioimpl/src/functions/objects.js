

module.exports = {

  async createBucket(event, context, client) {
    /*
      POST ?action=createBucket
      {
        "name": "test-bucket-1",
        "region": "us-east-1"
      }
    */
    const bucketName = event.payload.name
    const region = event.payload.region ? event.payload.region : 'us-east-1'

    client.makeBucket(bucketName, region, function(err) {
      if (err) return console.error(err)
      console.log('Bucket created successfully in "us-east-1".')
    })
  },

  async deleteBucket(event, context) {
    /*
      POST ?action=deleteBucket
      {
        "name": "test-bucket-1"
      }
    */
    const bucketName = event.payload.name
    minioClient.removeBucket(bucketName, function(err) {
      if (err) return console.log('unable to remove bucket.')
      console.log('Bucket removed successfully.')
    })
  },

  async listBuckets(event, context) {
    minioClient.listBuckets(function(err, buckets) {
      if (err) return console.log(err)
      console.log('buckets :', buckets)
    })
  },

  async bucketExists(event, context) {
    const bucketName = event.payload.name
    minioClient.bucketExists(bucketName, function(err, exists) {
      if (err) {
        return console.log(err)
      }
      if (exists) {
        return console.log('Bucket exists.')
      }
    })
  },

  async listObjects(event, context) {
    var stream = minioClient.listObjects('testbucket','', true)
    stream.on('data', function(obj) { console.log(obj) } )
    stream.on('error', function(err) { console.log(err) } )    
  },

  async removeObject(event, context) {    
    minioClient.removeObject('testbucket', 'test-object.jpg', function(err) {
      if (err) {
        return console.log('Unable to remove object', err)
      }
      console.log('Removed the object')
    })    
  }  
}