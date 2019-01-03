

module.exports = {

  /*
    POST ?action=bucketCreate
    {
      "name": "test-bucket-1",
      "region": "us-east-1"
    }
  */  
  async bucketCreate(event, context, client, cb) {
    const bucketName = event.actionPayload.name
    const region = event.actionPayload.region ? event.actionPayload.region : 'us-east-1'

    client.makeBucket(bucketName, region, function(err, bucket) {
      if (err) {
        console.error(err)
        return cb(errorResponse(err))
      }
      console.log(`Bucket "${bucketName}" created successfully in "${region}".`)
      cb(null, 
        output({ status: 'ok', message: `'${bucketName}' created successfully in '${region}'`}))
    })
  },

  /*
    POST ?action=bucketDelete
    {
      "name": "test-bucket-1"
    }
  */  
  async bucketDelete(event, context, client, cb) {
    const bucketName = event.actionPayload.name
    client.removeBucket(bucketName, function(err) {
      if (err) {
        console.error('unable to remove bucket.')
        
        cb(errorResponse(err))
      } else {
        console.log('Bucket removed successfully.')
        cb(null, 
          output({ status: 'ok', message: `'${bucketName}' was successfully deleted`}))
      }
    })
  },

  /*
    POST ?action=bucketExists
    {
      "name": "test-bucket-1"
    }
  */   
  async bucketExists(event, context, client, cb) {
    const bucketName = event.actionPayload.name
    client.bucketExists(bucketName, function(err, exists) {
      if (err) {
        console.error(err)
        cb(err)
      } else {
        cb(null, output({ bucket_exists: exists }))
      }
    })
  },

  /*
    POST ?action=bucketList
    [no payload]
   */
  async bucketsList(event, context, client, cb) {
    client.listBuckets(function(err, buckets) {
      if (err) {
        console.error(err)
        cb(err)
      } else {
        console.log('buckets :', buckets)
        cb(err, output(buckets))
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

function errorResponse(err) {
  return JSON.stringify({error: err.message}, null, 2)
}

function output(obj) {
  return JSON.stringify(obj, null, 2)
}