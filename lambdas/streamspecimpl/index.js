
exports.entryPoint = function(event, context, callback) {

  const contextData = JSON.parse(context)
  const eventData = JSON.parse(event)

  const handleEvents = async () => {

    switch(eventData.action) {
      case 'streamspec.create':
        console.log('STREAMSPEC.CREATE')
        create(eventData, contextData)
        break
      case 'streamspec.view':
        console.log('STREAMSPEC.VIEW')
        
        break
      case 'streamspec.update':
        console.log('STREAMSPEC.UPDATE')
        
        break
      case 'streamspec.delete':
        console.log('STREAMSPEC.DELETE')
        
        break
      case 'streamspec.start':
        console.log('STREAMSPEC.START')
        
        break
      default:
        console.log(`ERROR : Unknown event - ${eventData.action}`)  
        throw new Error(
          `Unhandled action '${eventData.action}'. The provider may be out of sync with this Lambda.`)   
    }
  }
}