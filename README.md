# people

A small Elm UI backed by the Acadia endpoints in `src/Backend.db`.

The [design system](DESIGN_SYSTEM.md) records our visual conventions, component
guidelines, and evolving design decisions. Update it as we refine the frontend.

For development, run:

```sh
make dev
```

This requires `watchexec`. Saving an Elm file recompiles `index.html` without
restarting Acadia. Saving an Acadia `.db` file recompiles the database modules,
regenerates the Elm bindings, recompiles `index.html`, and restarts the server.
If a rebuild fails, the watcher reports the compiler error and waits for the
next save. The server starts again after the next successful rebuild.
Refresh the browser to load the newly compiled page. Press Ctrl-C to stop both
watchers and the server.

To build or serve once instead:

```sh
make build
make serve
```

Then open <http://localhost:9000>. The page reads people from `getPeople` and
writes new ones with `addPerson` through Acadia's `/_endpoints` route. The Elm
application uses browser navigation, serves its `NewPerson` route at
<http://localhost:9000/people/new>, and renders `PageNotFound` for unknown URLs.
