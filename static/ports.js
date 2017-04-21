'use strict'
const Fs = require('fs');
const Path = require('path');
const expandHomeDir = require('expand-home-dir');
const Elm = require('./elm.js');
const DaemonConfig = expandHomeDir('~/.config/syncrypt/config');
const Electron = require('electron');
const File = require('file');

var readAuthToken = function(resolve, reject) {
  console.info('Trying to read auth token');
  Fs.readFile(DaemonConfig, 'utf-8', function (err, data) {
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

var openFolderDialog = function() {
  var folders = Electron.remote.dialog.showOpenDialog({properties: ['openDirectory']});
  if(folders && folders.length == 1) {
    var folderPath = folders[0].split(Path.sep);
    elmApp.ports.selectedFolder.send(folderPath)
  }
}

const IGNORE_FILES = [".DS_Store", ".vault"];

var getFileList = function(rootPathList) {
  var rootPath = rootPathList.join(Path.sep)
  File.walk(rootPath, function(_, dirPath, dirs, files) {
    var path = Path.relative(rootPath, dirPath).split(Path.sep);

    if(path[0] === ".vault") {
      return;
    }

    var data = [
      rootPathList,
      [
        path,
        files.map((x) => Path.basename(x)).filter((x) => x != ".DS_Store")
      ]
    ];

    elmApp.ports.fileList.send(data);
  })
}

var setupElmApp = function(daemonApiToken) {
  elmApp = Elm.Main.embed(mainContainer, {
    apiAuthToken: daemonApiToken,
    apiUrl: "http://127.0.0.1:28080/v1/",
    updateInterval: 5000
  });

  elmApp.ports.openFolder.subscribe(openFolderDialog)
  elmApp.ports.getFileList.subscribe(getFileList)
}

readAuthToken(setupElmApp)
