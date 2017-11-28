'use strict'
const electron = require('electron')

const app = electron.app // this is our app
const Menu = electron.Menu
const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows
const Path = require('path')
const ChildProcess = require('child_process')
const FileSystem = require('fs')

var mainWindow; // saves a global reference to mainWindow so it doesn't get garbage collected

const appPath = Path.dirname(app.getAppPath());
var daemonPath = Path.join(appPath, "app", "syncrypt", "syncrypt_daemon");

if (process.env.NODE_ENV === "development") {
  daemonPath = "client/syncrypt_daemon"
}


// Launch the Syncrypt daemon, if it exists
function launchDaemon() {

  if (process.platform == 'win32') daemonPath += '.exe';

  if (FileSystem.existsSync(daemonPath)) {
    const daemon = ChildProcess.spawn(daemonPath);

    daemon.on('data', (data) => {
      console.log("daemon: ", data)
    })

    daemon.on('close', (code) => {
      const msg = `child process exited with code ${code}`
      console.log(msg);
    });
  }
  else {
    console.warn('Did not start the daemon, because the following path does not ' +
      `exist: ${daemonPath}`)
  }
}

// This will create our app window, no surprise there
function createWindow() {
  var width = 832
  if (process.platform == 'win32') {
    width = 866
  }

  mainWindow = new BrowserWindow({
    width: width,
    minWidth: width,
    height: 750,
    minHeight: 750,
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
  })
}

// called when electron has initialized
app.on('ready', () => {
  launchDaemon()
  createWindow()
})

/* Mac Specific things */

// when you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

// if there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) {
    createWindow()
  }
})
