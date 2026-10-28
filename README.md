# People

People is an experiment in giving AI people identities, memories, and goals of
their own. You can create people, bring them into a conversation, and talk with
them—or let them talk to each other.

The project explores how an AI person's identity and past experiences shape
what they say and what they choose to do next. Alongside conversation, they can
save memories and create or complete personal goals. You can edit their profiles
and inspect what information went into each response.

## What you can do

- Create AI people with different identities and aspirations.
- Start a conversation with one or more people and join in yourself.
- Choose who speaks next, let the group exchange a set number of replies, or
  enable a new turn every five minutes.
- Read and edit each person's goals and memories as the conversation develops.
- Inspect a reply's prompts and actions to understand how it was produced.

The app includes two sample AI people, Ada and Sam, and a **Long-running
conversation** so you can try it without starting from scratch. Open that
conversation, select a person, and choose **Let selected person reply** once
the AI worker is running. Use **Stop** to end an automatic exchange.

## Current state

People is a local development prototype. It uses OpenAI for AI responses and
has one shared local human account.

**Conversation data lasts while the app's Acadia server is running.** Reloading
the browser or restarting the AI worker keeps your session. Restarting Acadia
resets people, messages, goals, and memories to the sample data. Persistent
storage and multiple user accounts are future work.

## Run it locally

You need Acadia 0.3.0, Elm, `watchexec`, Cabal, and GHC 9.8.4.

From the project root, start the app:

```sh
make dev
```

Open [localhost:9000](http://localhost:9000). You can browse and edit people
right away. AI replies need the separate worker described below.

### Enable AI replies

Set `OPENAI_API_KEY` and `OPENAI_MODEL` in `.env.local` or `.env`. If you do not
have either file, create one from the example:

```sh
cp .env.example .env
```

Choose a model that supports the OpenAI Responses API, structured outputs,
and function calling. Then, in a second terminal at the project root, run:

```sh
make worker
cabal run exe:people-worker -- run http://localhost:9000
```

Keep both terminals running while you use the app. AI replies use your OpenAI
account and incur API charges. Automatic replies start only when you enable
them. Restart the worker after changing its configuration.

## Working on the project

The interface is written in **Elm**. **Acadia** stores application data and
provides database operations. A **Haskell** worker selects speakers, builds
prompts, calls OpenAI, and saves responses.

During development, `make dev` rebuilds when files change. Refresh your browser
after an Elm change. Changes to `.db` files restart Acadia and reset the session;
also rebuild and restart the worker after those changes.

See the [development guide](DEVELOPMENT.md) for build commands, tests, private
sample data, and details of how AI turns work.

- [Code style](CODE_STYLE.md)
- [Design system](DESIGN_SYSTEM.md)
- [Vendored dependencies](third-party/README.md)
