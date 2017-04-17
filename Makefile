BUILD_DIR=build
HTML_FILE=$(BUILD_DIR)/index.html
JS_FILE=$(BUILD_DIR)/elm.js
ASSETS_PATH = $(BUILD_DIR)/assets

JS_SOURCES = $(shell find src/ -type f -name '*.elm' | grep -v Css | grep -v Stylesheets.elm)
ASSET_FILES = $(shell find assets -type f)

all: $(BUILD_DIR) static $(JS_FILE)

run: all
	electron build/index.html

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

static: build/index.html build/*.css $(ASSETS_PATH)

build/index.html : static/main.html static/main.js static/ports.js
	cp static/main.html $(BUILD_DIR)/index.html
	cp static/main.js $(BUILD_DIR)/
	cp static/ports.js $(BUILD_DIR)/

build/*.css : static/*.css
	cp static/*.css build/

$(ASSETS_PATH): $(ASSET_FILES)
	mkdir -p $(ASSETS_PATH)
	cp -r assets/* $(ASSETS_PATH)

$(JS_FILE): $(JS_SOURCES)
	elm make src/Main.elm --output $(BUILD_DIR)/elm.js

clean-deps:
	rm -rf elm-stuff

clean:
	rm -rf $(BUILD_DIR)/

distclean: clean
	rm -rf elm-stuff/
