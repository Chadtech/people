# people

A small Elm UI backed by the Acadia endpoints in `src/Backend.db`.

```sh
acadia make --gen-elm=generated
elm make src/Main.elm
acadia serve --html=index.html
```

Then open <http://localhost:9000>. The page reads foods with `getFoods` and
writes new ones with `addFood` through Acadia's `/_endpoints` route.
