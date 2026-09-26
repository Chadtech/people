.PHONY: build serve dev worker dev-data

dev-data:
	@test -f src/Fixtures.db || cp src/Fixtures.db.example src/Fixtures.db

build: dev-data
	acadia make --gen-elm=generated --gen-haskell=server/generated
	elm make src/Main.elm

serve: build
	acadia serve --html=index.html

dev:
	./scripts/dev

worker: build
	cabal build all
