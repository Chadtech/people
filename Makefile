.PHONY: build serve dev worker

build:
	acadia make --gen-elm=generated --gen-haskell=server/generated
	elm make src/Main.elm

serve: build
	acadia serve --html=index.html

dev:
	./scripts/dev

worker: build
	cabal build all
