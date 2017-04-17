'use strict'
const fs = require('fs');
const expandHomeDir = require('expand-home-dir');
const Elm = require('./elm.js');
const daemonConfig = expandHomeDir('~/.config/syncrypt/config');

var readAuthToken = function(resolve, reject) {
  console.info('Trying to read auth token');
  fs.readFile(daemonConfig, 'utf-8', function (err, data) {
    if (err) {
      reject();
      return;
    }
    const token = new RegExp("auth_token = ([a-zA-Z0-9]+)").exec(data);
    if (token && token.length == 2) {
      console.log("The auth token is: " + token[1]);
      resolve(token[1]);
    }
    else {
      reject();
    }
  });
}


// TODO: get this from syncrypt daemon config file
var mainContainer = window.document.getElementById("Root");
var elmApp = null;

readAuthToken(function(daemonApiToken) {
  elmApp = Elm.Main.embed(mainContainer, {
    apiAuthToken: daemonApiToken,
    apiUrl: "http://127.0.0.1:28080/v1/",
    updateInterval: 5000
  });
})
