const axios = require('axios')
const MetaClient = require('../common/client')

const reader = require('../common/reader')
const util = require('../common/util')
const debug = require('debug')('gestalt')

const u = require('util')

module.exports = {

  /**
   * 
   * @param {*} event 
   * @param {*} context 
   */
  eventDeploy(event, context) {
    let payload = event.actionPayload.org_fqon
    return { "status": "OK", "message": `deployed to ${payload}` }
  }
};