export PATH := ./node_modules/.bin:$(PATH)

# Database configuration
export DATASTORE_DATASET = purescript-datastore-proj
export DATASTORE_EMULATOR_HOST = localhost:8081
export DATASTORE_EMULATOR_HOST_PATH = localhost:8081/datastore
export DATASTORE_HOST = http://localhost:8081
export DATASTORE_PROJECT_ID = the-lab-173610


help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help


install:
	npm install
	bower install
.PHONY: install


test: ## Run the test watcher
	pulp test
.PHONY: test


test-watch: ## Run the tests once
	pulp --watch test
.PHONY: test-watch


build-watch: ## Incrementally compile the project
	pulp --watch build
.PHONY: build-watch


build: ## Compile the project once
	pulp build
.PHONY: build


db-start: ## Start the local GCP Datastore database
	gcloud beta emulators datastore start
.PHONY: db-start


clean: ## Delete compiled artefacts
	rm -fr output
.PHONY: clean
