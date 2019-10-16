'use strict'
const electron = require('electron')

const app = electron.app // this is our app
const Menu = electron.Menu
const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows
const Path = require('path')
const ChildProcess = require('child_process')
const FileSystem = require('fs')
const { autoUpdater, appUpdater } = require("electron-updater");
const Tray = electron.Tray
const os = require("os")

autoUpdater.logger = require("electron-log")
autoUpdater.logger.transports.file.level = "info"
autoUpdater.autoDownload = true
autoUpdater.channel = 'master'
// https://www.electron.build/auto-update#UpdateInfo
autoUpdater.setFeedURL({
  provider: 'generic',
  url: 'https://builds.syncrypt.space/updater/'
})

var mainWindow = null // saves a global reference to mainWindow so it doesn't get garbage collected
var systemTray = null
var daemon = null

const appPath = Path.dirname(app.getAppPath());
let daemonPath = Path.join(appPath, "app", "build", "syncrypt", "syncrypt_daemon");

if (process.env.NODE_ENV === "development") {
  daemonPath = Path.join("client", "syncrypt_daemon")
}


// Launch the Syncrypt daemon, if it exists
function launchDaemon() {
  if (process.platform == 'win32' && !daemonPath.includes(".exe")) {
    daemonPath += '.exe';
  }

  if (FileSystem.existsSync(daemonPath)) {
    daemon = ChildProcess.spawn(daemonPath)

    daemon.on('data', (data) => {
      console.log("daemon: ", data)
    })

    daemon.on('close', (code) => {
      const msg = `child process exited with code ${code}`
      console.log(msg)
    })
  } else {
    console.warn(
      `Did not start the daemon, because the following path does not exist: ${daemonPath}`
    )
  }
}


function restartDaemon() {
  console.log("Daemon restart requested")
  stopDaemon()
  launchDaemon()
}

function stopDaemon() {
  if (daemon) {
    console.log("Stopping daemon")
    daemon.kill()
    daemon = null
  }
}

let initiallyHideWindow = process.argv.indexOf("--hidden") > 0

// This will create our app window, no surprise there
function createWindow() {
  if (initiallyHideWindow) {
    initiallyHideWindow = false
    return
  }

  if (mainWindow) {
    mainWindow.focus()
    return
  }

  var width = 755
  if (process.platform == 'win32') {
    width += 34
  }

  mainWindow = new BrowserWindow({
    width: width,
    minWidth: 610,
    height: 820,
    minHeight: 410,
    title: "Syncrypt",
    frame: true,
    resizable: true,
    vibrancy: "dark"
  })

  // display the index.html file
  mainWindow.loadURL(`file://${__dirname}/index.html`)

  if (process.env.NODE_ENV === 'development') {
    mainWindow.openDevTools();
    mainWindow.webContents.on('context-menu', (e, props) => {
      const { x, y } = props;

      Menu.buildFromTemplate([{
        label: 'Inspect element',
        click() {
          mainWindow.inspectElement(x, y);
        }
      }]).popup(mainWindow);
    });
  }

  mainWindow.webContents.on('did-finish-load', () => {
    mainWindow.show();
    mainWindow.focus();
  });

  mainWindow.on('closed', function () {
    mainWindow = null
    if (process.platform === "darwin") {
      app.dock.hide()
    }
  })

  // when receiving a quitAndInstall signal, quit and install the new version
  electron.ipcMain.on("quitAndInstall", (event, arg) => {
    console.info("ipcMain received quitAndInstall");
    autoUpdater.quitAndInstall();
  })
}

function quitAll() {
  console.log("Quitting Syncrypt Desktop")
  stopDaemon()
  if (mainWindow) {
    mainWindow.close()
  }
  app.quit()
}

function initAutoUpdater() {
  setTimeout(() => {
    console.info("Checking for updates...")
    autoUpdater.checkForUpdates();
  }, 5000);
  autoUpdater.on('update-available', (info) => {
    autoUpdater.logger.info("Update available: " + JSON.stringify(info))
  });
  autoUpdater.on('update-downloaded', (info) => {
    console.info("Update downloaded: " + JSON.stringify(info))
    // Forward signal to webContents
    mainWindow.webContents.send('update-downloaded', info)
  });
}

const TRAY_ICON = "vault_tray_icon_16px.png"
const TRAY_ICON_CLICKED = "vault_tray_icon_bw_16px.png"

function createTray() {
  let trayIconPath = Path.join("assets", TRAY_ICON)
  if (process.env.NODE_ENV !== "development") {
    trayIconPath = Path.join(appPath, "app", "build", "assets", TRAY_ICON)
  }
  systemTray = new Tray(trayIconPath)
  const contextMenu = Menu.buildFromTemplate([
    { label: "Open Syncrypt Desktop", type: "normal", click: createWindow },
    { label: "Restart Daemon", type: "normal", click: restartDaemon },
    { label: "Stop Daemon", type: "normal", click: stopDaemon },
    { label: "Quit", type: "normal", click: quitAll },
  ])
  systemTray.setToolTip("Syncrypt Desktop")
  systemTray.setContextMenu(contextMenu)
  systemTray.setPressedImage(Path.join(Path.dirname(trayIconPath), TRAY_ICON_CLICKED))

  if (process.platform == 'win32') {
    systemTray.on("double-click", createWindow)
  }
}

const gotTheLock = app.requestSingleInstanceLock()

if (!gotTheLock) {
  app.quit()
} else {
  app.on('second-instance', (event, commandLine, workingDirectory) => {
    // A second instance has been opened, focus and/or restore the main window
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore()
      mainWindow.focus()
    }
  })

  // called when electron has initialized
  app.on('ready', () => {
    launchDaemon()
    createWindow()
    createTray()
    initAutoUpdater()
  })

  /* Mac Specific things */

  // when you close all the windows on a non-mac OS it quits the app
  app.on('window-all-closed', () => {
    // if (process.platform !== 'darwin') { app.quit() }
  })

  // if there is no mainWindow it creates one (like when you click the dock icon)
  app.on('activate', () => {
    if (mainWindow === null) {
      createWindow()
    }
  })
}
