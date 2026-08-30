.PHONY: build serve dev

build:
	acadia make --gen-elm=generated
	elm make src/Main.elm

serve: build
	acadia serve --html=index.html

dev:
	./scripts/dev
