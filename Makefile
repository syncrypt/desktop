all:
	mkdir -p build
	cp static/main.html build/index.html
	elm-css src/Stylesheets.elm --output build
	elm make src/Main.elm --output build/elm.js


clean:
	rm -rf build/
