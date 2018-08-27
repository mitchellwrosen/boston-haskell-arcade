[![Build Status](https://travis-ci.com/mitchellwrosen/boston-haskell-arcade.svg?branch=master)](https://travis-ci.com/mitchellwrosen/boston-haskell-arcade)

## Getting started

- Build the code with `cabal new-build`.
- Run the arcade with `cabal new-run`.

## Making a game

A terminal game is a program that:

- Receives input from the terminal, which includes key presses, resizes, and mouse clicks.
- Draws something to the screen.
- Ends at some point.

Events and drawing primitives are provided by the [termbox-banana](https://hackage.haskell.org/package/termbox-banana-0.1.0/docs/Termbox-Banana.html) library, and higher-level drawing functions can be found in [`Bha.View`](./src/Bha/View.hs).

There are currently two supported styles of game: Elm-style and Banana-style.

### Elm-style

An Elm game has four components.

- **init**. The initial model.

- **update**. The update loop. Given an event, which is either a tick or a terminal event, update the current model.

- **view**. Render the current model.

- **tickEvery**. Tick every `n` seconds, or not at all.

Here's what that looks like in Haskell.

```haskell
data ElmGame model
  = ElmGame
      (Seed -> model)
      (Either NominalDiffTime TerminalEvent -> StateT model Maybe ())
      (model -> Scene)
      (model -> Maybe NominalDiffTime)
```

### Banana style

A Banana game is written using the [reactive-banana](https://hackage.haskell.org/package/reactive-banana) FRP framework.

```haskell
type BananaGame
   = Events TermEvent
  -> Banana (Behavior Scene, Events ())
```

The `Banana` monad is a wrapper around `MomentIO`, used to prevent games from
performing arbitrary IO.

The game computation returns a time-varying scene to render, and an event that
fires when the game is over.

---

To contribute a game,

- Create a new module in the `Bha.Game.Impl.*` namespace.
- Implement the game in either Elm or Banana style.
- Add your game to the game list in [`Bha.Main`](./src/Bha/Main.hs).

Example games can be found at `src/Bha/Game/Impl/Example*`:

- [Elm Example 1](./src/Bha/Game/Impl/ElmExample.hs)
- [Banana Example 1](./src/Bha/Game/Impl/BananaExample.hs)

## Code organization

- `Bha.Main.*`

  The `main` modules. They manage which game to render, and where to route terminal events.

- `Bha.Game.Impl.*`

  The games.

- `Bha.Elm.*`

  Reusable Elm components.

- `Bha.Banana.*`

  Reusable FRP components. If you are building a game or UI in FRP style, you
  may find useful abstractions here.

- `Bha.View`

  High-level rendering helpers (not implemented yet). Rendering is still directly via the `termbox` API, which only supports setting individual cells.
