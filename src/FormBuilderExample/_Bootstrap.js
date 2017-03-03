'use strict';

exports.context = {
  init: function(settings) {
    var state = null;
    
    if (settings.env === 'dev') {}

    return state
  },
  ports: function(settings, app, state) {
  }
}
