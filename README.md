# Syncrypt Desktop in Elm

This is a WIP version of a rewrite of the Syncrypt Desktop UI in Elm.
Just me trying out Elm for now.

## Build target HTML, JS & CSS
    $ make
    # Generates build/index.html with JS & CSS

## Run automatically re-compiling web server
This doesn't generate the CSS on the fly yet and so it will look ugly.

    $ elm reactor
    # -> Go to http://localhost:8000 in your browser
