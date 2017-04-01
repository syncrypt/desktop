# Syncrypt Desktop in Elm

This is a WIP version of a rewrite of the Syncrypt Desktop UI in Elm.

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
