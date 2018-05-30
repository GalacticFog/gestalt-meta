
module.exports = {

  assert(any,msg){
    if (!any) {
      throw new Error(msg)
    } else {
      return any
    }
  },

  pretty(obj){ 
    JSON.stringify(obj, null, 2) 
  }

};