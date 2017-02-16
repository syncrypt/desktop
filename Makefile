BUILD_DIR=build

all:
	mkdir -p $(BUILD_DIR)
	cp static/main.html $(BUILD_DIR)/index.html
	elm-css src/Stylesheets.elm --output $(BUILD_DIR)
	elm make src/Main.elm --output $(BUILD_DIR)/elm.js


clean:
	rm -rf $(BUILD_DIR)/

distclean: clean
	rm -rf elm-stuff/
