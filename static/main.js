'use strict'
const electron = require('electron')

const app = electron.app // this is our app
const Menu = electron.Menu
const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows
const Path = require('path')
const ChildProcess = require('child_process')

var mainWindow; // saves a global reference to mainWindow so it doesn't get garbage collected

const appPath = Path.dirname(app.getAppPath());
var daemonPath = Path.join(appPath, "app", "syncrypt", "syncrypt_daemon");

if (process.env.NODE_ENV === "development") {
  daemonPath = "client/syncrypt_daemon"
}


app.on('ready', createWindow) // called when electron has initialized

// This will create our app window, no surprise there
function createWindow () {
  const daemon = ChildProcess.spawn(daemonPath);

  daemon.on('data', (data) => {
    console.log("daemon: ", data)
  })

  daemon.on('close', (code) => {
    const msg  = `child process exited with code ${code}`
    console.log(msg);
  });

  mainWindow = new BrowserWindow({
    width: 1024,
    minWidth: 900,
    height: 768,
    minHeight: 750,
    title: "Syncrypt",
    frame: true,
    resizable: true
  })

  // display the index.html file
  mainWindow.loadURL(`file://${ __dirname }/index.html`)

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

/* Mac Specific things */

// when you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

// if there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})
