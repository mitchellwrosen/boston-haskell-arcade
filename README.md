[![Build Status](https://travis-ci.com/mitchellwrosen/boston-haskell-arcade.svg?branch=master)](https://travis-ci.com/mitchellwrosen/boston-haskell-arcade)

### Getting started

- Build the code with `cabal new-build`.
- Run the arcade with `cabal new-run`.

### Making a game

A terminal game is a program that:

- Receives input from the terminal, which includes key presses, resizes, and mouse clicks.
- Draws something to the screen.
- Ends at some point.

There is currently one supported style of game: the Elm architecture.

An Elm game has five components.

- **init**. The initial model.

- **update**. The update loop. Given an event, which is either a tick or a terminal event, update the current model.

- **view**. Render the current model.

- **isDone**. Is the game over?

- **tickEvery**. Tick every `n` seconds, or not at all.

Here's what that looks like in Haskell.

```haskell
data ElmGame = forall model. ElmGame
  { init      :: model
  , update    :: Either NominalDiffTime Event -> model -> model
  , view      :: model -> Scene
  , isDone    :: model -> Bool
  , tickEvery :: model -> Maybe NominalDiffTime
  }
```

Events and drawing primitives are provided by the [termbox-banana](https://hackage.haskell.org/package/termbox-banana-0.1.0/docs/Termbox-Banana.html) library, and higher-level drawing functions can be found in [`Bha.View`](./src/Bha/View.hs).

To contribute a game,

- Create a new module in the `Bha.Game.Impl.*` namespace.
- Implement the five fields above.
- Add your game to the game list in [`Bha.Main`](./src/Bha/Main.hs).

Example games can be found at `src/Bha/Game/Impl/Example*`:

- [Elm Example 1](./src/Bha/Game/Impl/ElmExample.hs)

### Code organization

- `Bha.Main.*`

  The `main` modules. They manage which game to render, and where to route terminal events.

- `Bha.Game`

  Core game types. Individual games will import this module.

- `Bha.Game.Impl.*`

  The games.

- `Bha.Frp`

  Reusable FRP components. If you are building a game or UI in FRP style, you
  may find useful abstractions here.

- `Bha.View`

  High-level rendering helpers (not implemented yet). Rendering is still directly via the `termbox` API, which only supports setting individual cells.
