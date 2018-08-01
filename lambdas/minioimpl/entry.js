const crud = requires('./src/functions/objects')
const fn = new Map()

const contextData = JSON.parse(context);
const eventData = JSON.parse(event);
const eventName = eventData.action.toLowerCase()


fn.set('provider.createbucket', crud.createBucket)
fn.set('provider.removebucket', crud.removeBucket)
fn.set('provider.bucketexists', crud.bucketExists)
fn.set('provider.listbuckets', crud.listBuckets)


const process = (eventName) => {
  if (fn.has(eventName)) {
    fn.get(eventName)(eventData, contextData, callback)
  } else {
    callback(new Error(`Invalid event-name. found : '${eventName}'`))
  }
}

