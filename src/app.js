(function() {

'use strict';

require( './Stylesheets' );

var page = [ 'FormBuilderExample' ];

var settings =
  { env: 'dev'
  // { env: 'prod'
  };

var path = page.join('/');
var bootstrap = require( './' + path + '/_Bootstrap');
var ctrl = require('./' + path + '/Stub');
var i = 0;
var l = page.length;
while(i < l) {
  ctrl = ctrl[page[i]]; // Step down the relative path to the component
  i++
}
var ctx = bootstrap.context;
if (typeof ctx === 'undefined' || ctx === null) {
  ctx = {}
}
var state = null;
if (typeof ctx.init !== 'undefined' || ctx.init === null) {
  state = ctx.init(settings);
}

var app = ctrl.Stub.fullscreen(state);
if (typeof ctx.ports !== 'undefined' || ctx.init === null) {
  ctx.ports(settings, app, state);
}

})();
