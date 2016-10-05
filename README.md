# ElmOthello
the well known [Othello game](https://en.wikipedia.org/wiki/Reversi#) using Elm

![Othello](/Othello.png)

## Basics
All the basic modelling is inside the [Othello module](/Othello.elm) and [Main](/Main.elm) implements some very basic display using
`Div`s with columns of `Svg` elements (**TODO** add turn animations of some sort).

The game right now assumes **White** (first player) to be the human and moves the **black** computer-AI right after (as I could not
figure out if or how I can do long-running background processing with a `Task` or something similar).

## AI
The AI is based on version of the [Minimax algorithm](https://en.wikipedia.org/wiki/Minimax) implemented in the [MiniMax module](/MiniMax.elm).

The *heuristic* used in there is based on a very simple positional evaluation (implemented in the [AI module](/AI.elm)) - basically it
values the *corners* very high, the cells around the corner very low (indeed negative), all the edges and the center cells high, ...

```elm
coordValues =
    [ [ 10, -5, 6, 5, 5, 6, -5, 10 ]
    , [ -5, -8, -4, -4, -4, -4, -8, -5 ]
    , [ 6, -4, 2, 3, 3, 2, -4, 6 ]
    , [ 5, -4, 3, 4, 4, 3, -4, 6 ]
    , [ 5, -4, 3, 4, 4, 3, -4, 6 ]
    , [ 6, -4, 2, 3, 3, 2, -4, 6 ]
    , [ -5, -8, -4, -4, -4, -4, -8, -5 ]
    , [ 10, -5, 6, 5, 5, 6, -5, 10 ]
    ]
```

and will evaluate the algorithm to a *depth* of `3` (which yields quick answer times but is not really hard to beat).

If you want a stronger AI you probably will have to add some advanced strategy like valuing stone-count more at the end of the game,
valuing possible move-counts, pref switching few stones at the beginning etc. 

It's a fun task to make those algorithms better.

Of course you can try to implement [alpha-beta prunning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning) too - 
which I might or might not do myself in the future.
