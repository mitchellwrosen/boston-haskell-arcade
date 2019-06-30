[![Build Status](https://travis-ci.com/mitchellwrosen/boston-haskell-arcade.svg?branch=master)](https://travis-ci.com/mitchellwrosen/boston-haskell-arcade)

## Getting started

- Build the code with `./do build`.
- Build the haddocks with `./do docs`.
- Run the arcade with `./do run [server]`, where `server` is an optional host and
  port separated by a `:`, e.g. `127.0.0.1:8000`.

## Making a game

A terminal game is a program that:

- Receives input from the terminal, which includes key presses, resizes, and
  mouse clicks.
- Receives typed input from a game server (for multiplayer games, anyway).
- Can save and load files.
- Draws something to the screen.
- Ends at some point.

Events and drawing primitives are provided by the [termbox-banana](https://hackage.haskell.org/package/termbox-banana-0.1.0/docs/Termbox-Banana.html) library, and higher-level drawing functions can be found in [`Bha.View`](./src/Bha/View.hs).

There are currently two supported styles of game: Elm-style and Banana-style.

### Elm-style

To get started writing an Elm-style game, have a look at
[Elm Example 1](./src/Bha/Game/Impl/ElmExample.hs).

An Elm-style game has five components.

- **init**. The initial model. Computing the initial model occurs in the `Init`
  monad, which has access to randomness, storage, and multiplayer server
  communication.

- **update**. The update loop. Given an input event, update the current model.
  Computing updates occurs in the `Update` monad, which, like the `Init` monad,
  has access to randomness, storage, and multiplayer server communication.
  Additionally, it is a state monad that carries the model.

- **view**. Render the current model.

- **tickEvery**. Tick every `n` seconds, or not at all.

- **subscribe**. A set of topics to subscribe to, for multiplayer games.
  Messages published to each topic are broadcast to all subscribers. Single-
  player games will not subscribe to any topics.

Here's what that looks like in Haskell.

```haskell
data ElmGame model message
  = ElmGame
      (Init message model)
      (Input message -> Update model message ())
      (model -> Scene)
      (model -> Maybe Seconds)
      (model -> HashSet Text)

data Input message
  = Key Key
  | Mouse Mouse Int Int
  | Resize Int Int
  | Tick Seconds
  | Message Text message
```

### Banana style

To get started writing a Banana-style game, have a look at
[Banana Example 1](./src/Bha/Game/Impl/BananaExample.hs).

A Banana game is written using the [reactive-banana](https://hackage.haskell.org/package/reactive-banana) FRP framework.

```haskell
type BananaGame message
   = Events (Text, message)       -- Input from server
  -> Events TermEvent             -- Input from terminal
  -> Banana
       ( Behavior Scene           -- Scene to render
       , Behavior (HashSet Text)  -- Topics to subscribe to
       , Events (Text, message)   -- Messages to send to server
       , Events ()                -- Game over
       )
```

The `Banana` monad is a wrapper around `MomentIO`, used to restrict the kinds of IO that a game can perform.

The game computation returns a time-varying scene to render, time-varying set
of topics to subscribe to, an event that fires with messages to publish to the
server, and an event that fires when the game is over.

---

To contribute a game,

- Create a new module in the `Bha.Game.Impl.*` namespace.
- Implement the game in either Elm- or Banana-style.
- Add your game to the game list in [`Bha.Main`](./src/Bha/Main.hs).

## Code organization

- `Bha.Game.Impl.*`

  The games.

- `Bha.Elm.*`

  Hub for all things Elm. Poke around here when writing an Elm-style game.

- `Bha.Banana.*`

  Hub for all things Banana. Poke around here when writing a Banana-style game.

- `Bha.View`

  "High-level" rendering helpers (not implemented yet). Rendering is still
  directly via the `termbox` API, which only supports setting individual cells.
  This module is re-exported by `Bha.Elm.Prelude` and `Bha.Banana.Prelude`.

- `Bha.Main.*`

  The `main` modules. They manage which game to render, and where to route
  terminal events.

- `Interna.Bha.*`

  Random guts. You shouldn't have to look here to write a game.
