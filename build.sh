#!/bin/bash

mkdir -p build
rm -f build/*
cp static/main.html build/index.html
elm-css src/Stylesheets.elm --output build
elm make src/Main.elm --output build/elm.js
