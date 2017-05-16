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

var openFolderDialog = function(vaultId) {
  var folders = Electron.remote.dialog.showOpenDialog({properties: ['openDirectory']});
  if(folders && folders.length == 1) {
    var folderPath = folders[0].split(Path.sep);
    elmApp.ports.selectedFolder.send([vaultId, folderPath]);
  }
}

var openIconFileDialog = function(tag) {
  Electron.remote.dialog.showOpenDialog({
    properties: ["openFile"],
    title: "Select icon for vault",
    buttonLabel: "Select Icon",
    filters: [{name: "Images", extensions: ["jpg", "jpeg", "png", "gif"]}]
  }, (files) => {
    if(files && files.length == 1) {
      // var filePath = files[0].split(Path.sep);
      elmApp.ports.selectedIconFile.send([tag, files[0]]);
    }
  });
}

const IGNORE_FILES = [".DS_Store", ".vault"];

var getFileList = function([vaultId, rootPathList]) {
  var rootPath = rootPathList.join(Path.sep)
  File.walk(rootPath, function(_, dirPath, dirs, files) {
    var path = Path.relative(rootPath, dirPath).split(Path.sep);

    if(path[0] === ".vault") {
      return;
    }

    var data = [
      vaultId,
      rootPathList,
      [
        path,
        files.map((x) => Path.basename(x)).filter((x) => x != ".DS_Store")
      ]
    ];

    elmApp.ports.fileList.send(data);
  })
}

var focusOn = function(id) {
  var elem = document.getElementById(id);
  if(elem) {
    elem.focus();
  }
}

var openVaultFolder = function(path) {
  console.log("open folder:", path);
  if (process.platform === 'darwin') {
    Electron.shell.openExternal("file://" + path);
  } else {
    Electron.shell.openItem(path);
  }
}

var setupElmApp = function(daemonApiToken) {
  elmApp = Elm.Main.embed(mainContainer, {
    apiAuthToken: daemonApiToken,
    apiUrl: "http://127.0.0.1:28080/v1/",
    pathSeparator: Path.sep,
    updateInterval: 5000
  });

  elmApp.ports.openFolderDialog.subscribe(openFolderDialog)
  elmApp.ports.getFileList.subscribe(getFileList)
  elmApp.ports.focusOn.subscribe(focusOn)
  elmApp.ports.openVaultFolder.subscribe(openVaultFolder)
  elmApp.ports.openIconFileDialog.subscribe(openIconFileDialog)
}

readAuthToken(setupElmApp)
