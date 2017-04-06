# Syncrypt Desktop in Elm

This is a WIP version of a rewrite of the Syncrypt Desktop UI in Elm.
Syncrypt is a fully encrypted file storage service for groups and private
backups. This is a WIP rewrite of our platform independent GUI for the [Syncrypt
client](https://github.com/syncrypt/client).

## License

The source code for this desktop client is released under the GNU General Public
License Version 3. For more information have a look at the `LICENSE` file in this
directory. Additional information on the GNU GPLv3 can be found here:
http://www.gnu.org/licenses/quick-guide-gplv3.html

## Setup

Install Elm - see: https://guide.elm-lang.org/install.html

Install elm-github-install (needed for elm-ui):

    $ npm install elm-github-install -g

Install dependencies (including elm-ui) using elm-github-install:

    $ elm-install


## Build target HTML, JS & CSS
    $ make
    # Generates build/index.html with JS & CSS

## Set config to run it
For now, you'll need to set your API auth token in `static/main.html`.
You can find your API auth token in `~/.config/syncrypt/config`.
