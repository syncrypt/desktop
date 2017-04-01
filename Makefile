BUILD_DIR=build
HTML_FILE=$(BUILD_DIR)/index.html
JS_FILE=$(BUILD_DIR)/elm.js
CSS_FILE=$(BUILD_DIR)/index.css

JS_SOURCES = $(shell find src/ -type f -name '*.elm' | grep -v Css | grep -v Stylesheets.elm)
CSS_SOURCES = src/Stylesheets.elm $(shell find src/ -type f -name '*.elm' | grep -e Css)


all: $(BUILD_DIR) $(HTML_FILE) $(CSS_FILE) $(JS_FILE)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(HTML_FILE): static/main.html
	cp static/main.html $(BUILD_DIR)/index.html

$(JS_FILE): $(JS_SOURCES)
	elm make src/Main.elm --output $(BUILD_DIR)/elm.js

$(CSS_FILE): $(CSS_SOURCES)
	elm-css src/Stylesheets.elm --output $(BUILD_DIR)

clean-deps:
	rm -rf elm-stuff

clean:
	rm -rf $(BUILD_DIR)/

distclean: clean
	rm -rf elm-stuff/
