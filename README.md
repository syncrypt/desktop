# Syncrypt Desktop in Elm

This is a WIP version of a rewrite of the Syncrypt Desktop UI in Elm.
Just me trying out Elm for now.

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
You need to set your API auth token in `Config.initialConfig`.
Edit `src/Config.elm` before trying to run it, it won't work without that.
You can find your API auth token in `~/.config/syncrypt/config`.

## Run automatically re-compiling web server
This doesn't generate the CSS on the fly yet and so it will look ugly.

    $ elm reactor
    # -> Go to http://localhost:8000 in your browser
