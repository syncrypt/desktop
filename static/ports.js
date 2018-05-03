'use strict'
const Fs = require('fs');
const Path = require('path');
const expandHomeDir = require('expand-home-dir');
const Elm = require('./elm.js');
const DaemonConfig = expandHomeDir('~/.config/syncrypt/config');
const Electron = require('electron');
const File = require('file');

const mainContainer = window.document.getElementById("Root");
var elmApp = null;

const readAuthToken = function (resolve, reject) {
  Fs.readFile(DaemonConfig, 'utf-8', function (err, data) {
    if (err) {
      reject();
      return;
    }
    const token = new RegExp("auth_token = ([a-zA-Z0-9]+)").exec(data);
    if (token && token.length == 2) {
      resolve(token[1]);
    }
    else {
      reject();
    }
  });
}


const openFolderDialog = function (vaultId) {
  var folders = Electron.remote.dialog.showOpenDialog({ properties: ["openDirectory", "createDirectory"] });
  if (folders && folders.length == 1) {
    var folderPath = folders[0].split(Path.sep);
    elmApp.ports.selectedFolder.send([vaultId, folderPath]);
  }
}

const openIconFileDialog = function (tag) {
  Electron.remote.dialog.showOpenDialog({
    properties: ["openFile"],
    title: "Select icon for vault",
    buttonLabel: "Select Icon",
    filters: [{ name: "Images", extensions: ["jpg", "jpeg", "png"] }]
  }, (files) => {
    if (files && files.length == 1) {
      var iconUrl = Electron.nativeImage.createFromPath(files[0]).resize({
        width: 100,
        height: 100,
        quality: "good"
      }).toDataURL();
      elmApp.ports.selectedIconFile.send([tag, iconUrl]);
    }
  });
}

const openExportFileDialog = function ([tag, buttonLabel]) {
  Electron.remote.dialog.showSaveDialog({
    title: "Select file to export Vault key bundle to",
    buttonLabel: buttonLabel,
    filters: [{ name: "Vault Export Archives", extensions: ["zip"] }]
  }, (file) => {
    elmApp.ports.selectedExportFile.send([tag, file]);
  });
}

const IGNORE_FILES = [".DS_Store", ".vault"];

const getFileList = function ([vaultId, rootPathList]) {
  var rootPath = rootPathList.join(Path.sep)
  File.walk(rootPath, function (_, dirPath, dirs, files) {
    var path = Path.relative(rootPath, dirPath).split(Path.sep);

    if (isVaultPath(path)) {
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

const isVaultPath = function (path) {
  for (var i = 0; i < path.length; i++) {
    if (path[i] == ".vault") {
      return true;
    }
  }
  return false;
}

const focusOn = function (id) {
  var elem = document.getElementById(id);
  if (elem) {
    elem.focus();
  }
}

const openVaultFolder = function (path) {
  console.log("open folder:", path);
  if (process.platform === 'darwin') {
    Electron.shell.openExternal("file://" + path);
  } else {
    Electron.shell.openItem(path);
  }
}

const addEmailToCompletionList = function (email) {
  var emails = Array(localStorage["EmailCompletionList"])

  if (emails.includes(email)) {
    return;
  }

  emails.push(email)
  localStorage["EmailCompletionList"] = emails
}

const updateEmailCompletionList = function () {
  var emails = localStorage["EmailCompletionList"]
  if (emails) {
    elmApp.ports.getEmailCompletionList.send(Array(emails))
  }
}

const openPasswordResetInBrowser = () => {
  if (process.platform === 'darwin') {
    Electron.shell.openExternal("https://alpha.syncrypt.space/password_reset");
  } else {
    Electron.shell.openItem("https://alpha.syncrypt.space/password_reset");
  }
}

const openUserKeyExportFileDialog = (buttonLabel) => {
  Electron.remote.dialog.showSaveDialog({
    title: "Select file to export your key to",
    buttonLabel: buttonLabel,
    filters: [{ name: "User Key Export Archive", extensions: ["zip"] }]
  }, (file) => {
    const filePath = file.split(Path.sep)
    elmApp.ports.selectedUserKeyExportFile.send(filePath);
  });
}

const quitAndInstall = () => {
  Electron.ipcRenderer.send('quitAndInstall');
}

const getEnvLocale = () => {
  const env = process.env
  return env.LC_ALL || env.LC_MESSAGES || env.LANG || env.LANGUAGE || "en_US.UTF-8"
}

const openVaultKeyImportFileDialog = function (tag) {
  Electron.remote.dialog.showOpenDialog({
    properties: ["openFile"],
    title: "Select vault key",
    buttonLabel: "Import Vault Key",
    filters: [{ name: "Vault Keys", extensions: ["zip"] }]
  }, (files) => {
    if (files && files.length == 1) {
      const filePath = files[0].split(Path.sep)
      elmApp.ports.selectedVaultKeyImportFile.send(filePath)
    }
  })
}

const openVaultImportFolderDialog = function () {
  var folders = Electron.remote.dialog.showOpenDialog({ properties: ["openDirectory", "createDirectory"] });
  if (folders && folders.length == 1) {
    const folderPath = folders[0].split(Path.sep)
    elmApp.ports.selectedVaultImportFolder.send(folderPath)
  }
}

const setupElmApp = function (daemonApiToken) {
  elmApp = Elm.Main.embed(mainContainer, {
    apiAuthToken: daemonApiToken,
    apiUrl: "http://127.0.0.1:28080/v1/",
    pathSeparator: Path.sep,
    updateInterval: 3000,
    version: Electron.remote.app.getVersion(),
    locale: getEnvLocale()
  });

  elmApp.ports.openFolderDialog.subscribe(openFolderDialog)
  elmApp.ports.getFileList.subscribe(getFileList)
  elmApp.ports.focusOn.subscribe(focusOn)
  elmApp.ports.openVaultFolder.subscribe(openVaultFolder)
  elmApp.ports.openIconFileDialog.subscribe(openIconFileDialog)
  elmApp.ports.openExportFileDialog.subscribe(openExportFileDialog)
  elmApp.ports.addEmailToCompletionList.subscribe(addEmailToCompletionList)
  elmApp.ports.updateEmailCompletionList.subscribe(updateEmailCompletionList)
  elmApp.ports.openPasswordResetInBrowser.subscribe(openPasswordResetInBrowser)
  elmApp.ports.openUserKeyExportFileDialog.subscribe(openUserKeyExportFileDialog)
  elmApp.ports.quitAndInstall.subscribe(quitAndInstall)
  elmApp.ports.openVaultKeyImportFileDialog.subscribe(openVaultKeyImportFileDialog)
  elmApp.ports.openVaultImportFolderDialog.subscribe(openVaultImportFolderDialog)

  Electron.ipcRenderer.on('update-downloaded', (ev, info) => {
    elmApp.ports.updateAvailable.send(info.version)
  })
}

readAuthToken(setupElmApp)
