# people

A small Elm UI backed by the Acadia endpoints in `src/Backend.db`.

For development, run:

```sh
make dev
```

This requires `watchexec`. Saving an Elm file recompiles `index.html` without
restarting Acadia. Saving an Acadia `.db` file recompiles the database modules,
regenerates the Elm bindings, recompiles `index.html`, and restarts the server.
Refresh the browser to load the newly compiled page. Press Ctrl-C to stop both
watchers and the server.

To build or serve once instead:

```sh
make build
make serve
```

Then open <http://localhost:9000>. The page reads people from `getPeople` and
writes new ones with `addPerson` through Acadia's `/_endpoints` route.
