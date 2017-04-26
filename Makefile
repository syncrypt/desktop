BUILD_DIR=build
RELEASE_DIR=release
HTML_FILE=$(BUILD_DIR)/index.html
JS_FILE=$(BUILD_DIR)/elm.js
ASSETS_PATH = $(BUILD_DIR)/assets

JS_SOURCES = $(shell find src/ -type f -name '*.elm')
ASSET_FILES = $(shell find assets -type f)
MAIN_FILE = $(BUILD_DIR)/main.js

all: $(BUILD_DIR) static $(JS_FILE)

run: all
	NODE_ENV=development electron $(MAIN_FILE)

run-prod: all
	NODE_ENV=production electron $(MAIN_FILE)

run-debug: $(BUILD_DIR) static
	elm make src/Main.elm --output $(JS_FILE) --debug
	NODE_ENV=development electron $(MAIN_FILE)

release-setup: all
	rm -rf $(RELEASE_DIR)/tmp/*
	mkdir -p $(RELEASE_DIR)/tmp/syncrypt
	cp -r $(BUILD_DIR)/* $(RELEASE_DIR)/tmp/syncrypt
	cp package.json $(RELEASE_DIR)/tmp/
	sed -i -e "s/build\/main/syncrypt\/main/g" $(RELEASE_DIR)/tmp/package.json
	cp client/* $(RELEASE_DIR)/tmp/syncrypt/
	cd $(RELEASE_DIR)/tmp && npm install

release: release-setup
	#cd $(BUILD_DIR) && electron-packager ./ Syncrypt --overwrite
	cd $(RELEASE_DIR)/tmp && \
		npm run make-installer
	mv $(RELEASE_DIR)/tmp/out/* $(RELEASE_DIR)/
	rm -rf ./$(RELEASE_DIR)/tmp/*

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

static: $(BUILD_DIR)/index.html $(BUILD_DIR)/*.css $(ASSETS_PATH)

$(BUILD_DIR)/index.html : static/main.html static/main.js static/ports.js
	cp static/main.html $(BUILD_DIR)/index.html
	cp static/main.js $(BUILD_DIR)/
	cp static/ports.js $(BUILD_DIR)/

$(BUILD_DIR)/*.css : static/*.css
	cp static/*.css $(BUILD_DIR)

$(ASSETS_PATH): $(ASSET_FILES)
	mkdir -p $(ASSETS_PATH)
	cp -r assets/* $(ASSETS_PATH)

$(JS_FILE): $(JS_SOURCES)
	elm make src/Main.elm --output $(BUILD_DIR)/elm.js $(DEBUG)

clean-deps:
	rm -rf elm-stuff

clean:
	rm -rf $(BUILD_DIR)/
	rm -rf $(RELEASE_DIR)/tmp/

distclean: clean
	rm -rf elm-stuff/
