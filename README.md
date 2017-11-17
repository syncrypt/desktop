# Syncrypt Desktop in Elm

This is our next iteration of the Syncrypt Desktop UI written in Elm.
Syncrypt is a fully client-side, end-to-end encrypted file storage service
for groups and private backups. This is our platform independent GUI for the
[Syncrypt client](https://github.com/syncrypt/client).

## Download

You can download the latest supported Syncrypt releases from our [official releases
page](http://alpha.syncrypt.space/releases/). Note that Syncrypt is currently
in closed alpha. You can get your alpha invite @ [syncrypt.space](https://syncrypt.space).

## License

The source code for this desktop client is released under the GNU General Public
License Version 3. For more information have a look at the `LICENSE` file in this
directory. Additional information on the GNU GPLv3 can be found here:
http://www.gnu.org/licenses/quick-guide-gplv3.html

## Setup

Install Elm - see: https://guide.elm-lang.org/install.html

Install elm-github-install (needed for elm-ui and other custom packages with
native code):

    $ npm install elm-github-install -g

Install elm dependencies (including elm-ui) using elm-github-install:

    $ elm-install

Install JS dependencies (mostly for building electron release packages):

    $ npm install

## Build target HTML, JS & CSS
    $ make
    # Generates build/index.html with JS & CSS

## Run with Electron
    $ make run

## Build release for current platform

    $ make release
