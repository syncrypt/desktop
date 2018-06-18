'use strict'
const Fs = require('fs')
const Path = require('path')
const expandHomeDir = require('expand-home-dir')
const Elm = require('./elm.js')
const DaemonConfig = expandHomeDir('~/.config/syncrypt/config')
const Electron = require('electron')
const File = require('file')
const AutoLaunch = require("auto-launch")

const mainContainer = window.document.getElementById("Root")
var elmApp = null

const toElmPath = (pathString) =>
  pathString.split(Path.sep)

const readAuthToken = (resolve, reject) => {
  Fs.readFile(DaemonConfig, 'utf-8', (err, data) => {
    if (err) {
      reject()
      return
    }
    const token = new RegExp("auth_token = ([a-zA-Z0-9]+)").exec(data)
    if (token && token.length == 2) {
      resolve(token[1])
    }
    else {
      reject()
    }
  })
}


const openFolderDialog = (vaultId) => {
  const folders = Electron.remote.dialog.showOpenDialog({ properties: ["openDirectory", "createDirectory"] })
  if (folders && folders.length == 1) {
    elmApp.ports.selectedFolder.send([
      vaultId,
      toElmPath(folders[0])
    ])
  }
}

const openIconFileDialog = (tag) => {
  Electron.remote.dialog.showOpenDialog({
    properties: ["openFile"],
    title: "Select icon for vault",
    buttonLabel: "Select Icon",
    filters: [{ name: "Images", extensions: ["jpg", "jpeg", "png"] }]
  }, (files) => {
    if (files && files.length == 1) {
      const iconUrl = Electron.nativeImage.createFromPath(files[0]).resize({
        width: 100,
        height: 100,
        quality: "good"
      }).toDataURL()
      elmApp.ports.selectedIconFile.send([tag, iconUrl])
    }
  })
}

const openExportFileDialog = ([tag, buttonLabel]) => {
  Electron.remote.dialog.showSaveDialog({
    title: "Select file to export Vault key bundle to",
    buttonLabel: buttonLabel,
    filters: [{ name: "Vault Export Archives", extensions: ["zip"] }]
  }, (file) => {
    elmApp.ports.selectedExportFile.send([tag, file])
  })
}

const IGNORE_FILES = [".DS_Store", ".vault"]

const getFileList = ([vaultId, rootPathList]) => {
  const rootPath = rootPathList.join(Path.sep)
  File.walk(rootPath, (_, dirPath, dirs, files) => {
    const path = toElmPath(Path.relative(rootPath, dirPath))

    if (isVaultPath(path)) {
      return
    }

    const data = [
      vaultId,
      rootPathList,
      [
        path,
        files.map((x) => Path.basename(x)).filter((x) => x != ".DS_Store")
      ]
    ]

    elmApp.ports.fileList.send(data)
  })
}

const isVaultPath = (path) => {
  for (let i = 0; i < path.length; i++) {
    if (path[i] === ".vault") {
      return true
    }
  }
  return false
}

const focusOn = (id) => {
  const elem = document.getElementById(id)
  if (elem) {
    elem.focus()
  }
}

const openVaultFolder = (path) => {
  if (process.platform === 'darwin') {
    Electron.shell.openExternal("file://" + path)
  } else {
    Electron.shell.openItem(path)
  }
}

const addEmailToCompletionList = (email) => {
  const emails = Array(localStorage["EmailCompletionList"])

  if (emails.includes(email)) {
    return
  }

  emails.push(email)
  localStorage["EmailCompletionList"] = emails
}

const updateEmailCompletionList = () => {
  const emails = localStorage["EmailCompletionList"]
  if (emails) {
    elmApp.ports.getEmailCompletionList.send(Array(emails))
  }
}

const openPasswordResetInBrowser = () => {
  if (process.platform === 'darwin') {
    Electron.shell.openExternal("https://alpha.syncrypt.space/password_reset")
  } else {
    Electron.shell.openItem("https://alpha.syncrypt.space/password_reset")
  }
}

const openUserKeyExportFileDialog = (buttonLabel) => {
  Electron.remote.dialog.showSaveDialog({
    title: "Select file to export your key to",
    buttonLabel: buttonLabel,
    filters: [{ name: "User Key Export Archive", extensions: ["zip"] }]
  }, (file) => {
    elmApp.ports.selectedUserKeyExportFile.send(toElmPath(file))
  })
}

const quitAndInstall = () => {
  Electron.ipcRenderer.send('quitAndInstall')
}

const getEnvLocale = () => {
  const env = process.env
  return env.LC_ALL || env.LC_MESSAGES || env.LANG || env.LANGUAGE || "en_US.UTF-8"
}

const openVaultKeyImportFileDialog = (tag) => {
  Electron.remote.dialog.showOpenDialog({
    properties: ["openFile"],
    title: "Select vault key",
    buttonLabel: "Import Vault Key",
    filters: [{ name: "Vault Keys", extensions: ["zip"] }]
  }, (files) => {
    if (files && files.length == 1) {
      elmApp.ports.selectedVaultKeyImportFile.send(toElmPath(files[0]))
    }
  })
}

const openVaultImportFolderDialog = () => {
  const folders = Electron.remote.dialog.showOpenDialog({ properties: ["openDirectory", "createDirectory"] })
  if (folders && folders.length == 1) {
    elmApp.ports.selectedVaultImportFolder.send(toElmPath(folders[0]))
  }
}

const syncryptAutoLauncher = new AutoLaunch({
  name: "space.syncrypt.daemon",
  isHidden: true
})

const enableAutoStart = () => {
  console.log("enableAutoStart")
  syncryptAutoLauncher.enable()
  syncryptAutoLauncher.isEnabled()
    .then((isEnabled) => {
      elmApp.ports.autoStartChanged.send(true)
      if (isEnabled) {
        return
      }
      syncryptAutoLauncher.enable()
    })
    .catch(function (err) {
      console.error("Got error in enableAutoStart:", err)
    })
}

const updateAutoStartEnabledState = () => {
  console.log("updateAutoStartEnabledState")
  syncryptAutoLauncher.isEnabled()
    .then((isEnabled) => {
      console.log("updateAutoStartEnabledState:", isEnabled)
      elmApp.ports.autoStartChanged.send(isEnabled)
    })
    .catch(function (err) {
      console.error("Got error in updateAutoStartEnabledState:", err)
      setTimeout(updateAutoStartEnabledState, 500)
    })
}

const disableAutoStart = () => {
  console.log("disableAutoStart")
  syncryptAutoLauncher.disable()
  syncryptAutoLauncher.isEnabled()
    .then((isEnabled) => {
      elmApp.ports.autoStartChanged.send(false)
      if (isEnabled) {
        syncryptAutoLauncher.disable()
      }
    })
    .catch(function (err) {
      console.error("Got error in disableAutoStart:", err)
    })
}

const setupElmApp = (daemonApiToken) => {
  elmApp = Elm.Main.embed(mainContainer, {
    apiAuthToken: daemonApiToken,
    apiUrl: "http://127.0.0.1:28080/v1/",
    pathSeparator: Path.sep,
    updateInterval: 3000,
    version: Electron.remote.app.getVersion(),
    locale: getEnvLocale()
  })

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
  elmApp.ports.enableAutoStart.subscribe(enableAutoStart)
  elmApp.ports.updateAutoStartEnabledState.subscribe(updateAutoStartEnabledState)
  elmApp.ports.disableAutoStart.subscribe(disableAutoStart)

  Electron.ipcRenderer.on('update-downloaded', (ev, info) => {
    elmApp.ports.updateAvailable.send(info.version)
  })
}

readAuthToken(setupElmApp)
