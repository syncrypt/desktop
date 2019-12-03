BUILD_DIR=build
RELEASE_DIR=release
PACKAGE_DIR=dist
HTML_FILE=$(BUILD_DIR)/index.html
JS_FILE=$(BUILD_DIR)/elm.js
ASSETS_PATH = $(BUILD_DIR)/assets

JS_SOURCES = $(wildcard src/*.elm) $(wildcard src/**/*.elm)
ASSET_FILES = $(wildcard assets/*)
MAIN_FILE = $(BUILD_DIR)/main.js
CSS_FILES = $(wildcard static/*.scss)
CSS_TARGETS = $(subst static,build,$(CSS_FILES:.scss=.css))

ELECTRON_BUILD=./node_modules/.bin/build

SASS_CMD=./node_modules/node-sass/bin/node-sass

all: $(BUILD_DIR) static $(JS_FILE)

run: all
	NODE_ENV=development electron $(MAIN_FILE)

run-prod: all
	NODE_ENV=production electron $(MAIN_FILE)

run-debug: $(BUILD_DIR) static
	elm make src/Main-Debugger.elm --output $(JS_FILE) --debug
	NODE_ENV=development electron $(MAIN_FILE)
	rm $(JS_FILE)

run-watch: all
	$(SASS_CMD) --watch --recursive --output build/ --source-map true --source-map-contents static/ &
	NODE_ENV=development electron $(MAIN_FILE) &
	npm run watch

watch-css: all
	$(SASS_CMD) --watch --recursive --output build/ --source-map true --source-map-contents static/

package-setup: all
	cp icon.* $(BUILD_DIR)
	cp package.json $(BUILD_DIR)
	sed -i -e "s/build\/main/main/g" $(BUILD_DIR)/package.json
	cd $(BUILD_DIR) && npm install --no-save --production

release-setup: all
	rm -rf $(RELEASE_DIR)/tmp/*
	mkdir -p $(RELEASE_DIR)/tmp/syncrypt
	mkdir -p $(RELEASE_DIR)/tmp/build
	cp icon.* $(RELEASE_DIR)/tmp/
	cp -r $(BUILD_DIR)/* $(RELEASE_DIR)/tmp/syncrypt
	cp package.json $(RELEASE_DIR)/tmp/
	sed -i -e "s/build\/main/syncrypt\/main/g" $(RELEASE_DIR)/tmp/package.json
	sed -i -e "s/\"electron-forge\": \"^4.0.2\",//g" $(RELEASE_DIR)/tmp/package.json
	cp client/* $(RELEASE_DIR)/tmp/syncrypt/
	cd $(RELEASE_DIR)/tmp && npm install --no-save

release: release-setup
	#cd $(BUILD_DIR) && electron-packager ./ Syncrypt --overwrite
	cd $(RELEASE_DIR)/tmp && \
		npm run make-installer
	mv $(RELEASE_DIR)/tmp/out/make/* $(RELEASE_DIR)/

# Linux builds

Syncrypt-Desktop-linux.zip: all
	rm -rf $(PACKAGE_DIR) $@
	$(ELECTRON_BUILD) --linux zip
	cp $(PACKAGE_DIR)/Syncrypt-Desktop-linux.zip $@

Syncrypt-Desktop.AppImage: all
	rm -rf $(PACKAGE_DIR) $@
	$(ELECTRON_BUILD) --linux AppImage
	cp $(PACKAGE_DIR)/Syncrypt-Desktop.AppImage $@

# Darwin builds

Syncrypt-Desktop-darwin.zip: all
	rm -rf $(PACKAGE_DIR) $@
	$(ELECTRON_BUILD) --mac zip
	cp $(PACKAGE_DIR)/Syncrypt-Desktop-mac.zip $@

Syncrypt-Desktop.dmg: all
	rm -rf $(PACKAGE_DIR) $@
	$(ELECTRON_BUILD) --mac dmg
	cp $(PACKAGE_DIR)/Syncrypt-Desktop.dmg $@

# Windows builds

Syncrypt-Desktop-win32.zip: all
	rm -rf $(PACKAGE_DIR) $@
	$(ELECTRON_BUILD) --win zip
	cp $(PACKAGE_DIR)/Syncrypt-Desktop-win.zip $@

Syncrypt-Desktop-Setup.exe: all
	rm -rf $(PACKAGE_DIR) $@
	$(ELECTRON_BUILD) --win nsis
	cp $(PACKAGE_DIR)/Syncrypt-Desktop-Setup.exe $@


$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

static: $(BUILD_DIR)/index.html $(CSS_TARGETS) $(ASSETS_PATH)

$(BUILD_DIR)/index.html : static/main.html static/main.js static/ports.js
	cp static/main.html $(BUILD_DIR)/index.html
	cp static/main.js $(BUILD_DIR)/
	cp static/ports.js $(BUILD_DIR)/

build/%.css: static/%.scss
	$(SASS_CMD) $< $@

$(ASSETS_PATH): $(ASSET_FILES)
	mkdir -p $(ASSETS_PATH)
	cp -r assets/* $(ASSETS_PATH)

$(JS_FILE): $(JS_SOURCES)
	elm make src/Main.elm --output $(BUILD_DIR)/elm.js $(DEBUG)

test-setup:
	cd tests && elm-install

test:
	elm-test

deps:
	npm install --no-save && node_modules/.bin/elm-install

clean-deps:
	rm -rf elm-stuff
	rm -rf node_modules/

clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(PACKAGE_DIR)
	rm -rf $(RELEASE_DIR)/tmp/
	rm -rf Syncrypt-Desktop-linux.zip Syncrypt-Desktop-darwin.zip Syncrypt-Desktop-win32.zip

distclean: clean clean-deps
	rm -rf $(RELEASE_DIR)

version:
	@node -p "require('./package.json').version"
