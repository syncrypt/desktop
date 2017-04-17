BUILD_DIR=build
HTML_FILE=$(BUILD_DIR)/index.html
JS_FILE=$(BUILD_DIR)/elm.js
CSS_FILE=$(BUILD_DIR)/index.css
ASSETS_PATH = $(BUILD_DIR)/assets

JS_SOURCES = $(shell find src/ -type f -name '*.elm' | grep -v Css | grep -v Stylesheets.elm)
CSS_SOURCES = src/Css/Stylesheets.elm $(shell find src/ -type f -name '*.elm' | grep -e Css)
ASSET_FILES = $(shell find assets -type f)

all: $(BUILD_DIR) $(HTML_FILE) $(JS_FILE) $(ASSETS_PATH)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(ASSETS_PATH): $(ASSET_FILES)
	mkdir -p $(ASSETS_PATH)
	cp -r assets/* $(ASSETS_PATH)

$(HTML_FILE): static/main.html
	cp static/main.html $(BUILD_DIR)/index.html
	cp static/*.css $(BUILD_DIR)/

$(JS_FILE): $(JS_SOURCES)
	elm make src/Main.elm --output $(BUILD_DIR)/elm.js

clean-deps:
	rm -rf elm-stuff

clean:
	rm -rf $(BUILD_DIR)/

distclean: clean
	rm -rf elm-stuff/
