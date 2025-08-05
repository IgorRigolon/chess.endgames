# chess.endgames
An interactive data visualization to see how tablebase positions play out in practice

Click on the image below to open the data visualization. You can filter games by time control, rating range, and by either material count or a specific FEN. Tablebase evaluations and results are always shown from white's perspective. _Note: It runs locally on your browser, so it may slow down your computer._

![Click here to see](examples/main.png)

## Introduction

Even though chess positions with 7 pieces or fewer are solved by computers, they're not always simple for humans to play correctly. I analyze over 91 million games played on [lichess](lichess.org) to answer the question:

> How often do players mess up objectively winning or drawn endgames?

Out of over 91 million games analyzed, 4.76 million of them reached a tablebase position (3-4-5 piece tablebase). That amounts to 76.2 million tablebase FENs.

## Filtering by material

You can select any material imbalance where there are fewer than 5 pieces on the board (kings included). For example, you can search for positions where white has a Bishop + Knight and black only has a King.

![](examples/bishop_knight.png)

## Filtering by position

To search a specific position, you can set it up on the [Lichess board editor](https://lichess.org/editor/) and paste its FEN. Below I've searched for a [Lucena position](https://lichess.org/editor/1K1k4/1P6/8/8/8/8/r7/2R5_w_-_-_0_1?color=white), specifically the one shown on the [Wikipedia page](https://en.wikipedia.org/wiki/Lucena_position). You could also add more FENs to include more analogous positions.

![](examples/lucena.png)

## Data

The data I used comes from the [lichess database](https://database.lichess.org/) and from [Syzygy tablebases](https://syzygy-tables.info/#download). If you just want to look at the processed data, it's all in the `/data/` folder. If you clone the repository, you can open it in R with

```
library(arrow)

dat <- open_dataset(".../chess.endgames/data/")
```

or with analogous code on Python or other languages. Just beware that the data is probably too big to load into memory all at once, which is why [Arrow](https://arrow.apache.org/) comes in handy. You'll probably want to query a subset of the data or summary statistics before running `collect()` on it to actually open it.

## Workflow

I manually downloaded the games played in June 2025 from the [lichess database](https://database.lichess.org/) and did three things with them: (i) I parsed the PGNs into data frames, (ii) I generated all the FENs reached in the games and checked which are tablebase positions, and (iii) I probed a locally-downloaded 3-4-5 piece [Syzygy tablebase](https://syzygy-tables.info/#download) to get the tablebase evaluations of positions. Even though I did everything in R, these last two steps relied heavily on the [`python-chess` library](https://pypi.org/project/chess/). The whole database took around a week to fully process on my weak computer.

The `process_database.R` script receives a big lichess database file and spits out the tablebase positions found within them, along with the ratings of players and the actual result of the games. Other scripts in the `/scripts/` folder are auxiliary. The output of this script is stored in `/data-raw/`, where each chunk of games is stored in a separate `.parquet` file. Once all chunks are done processing, they're written into a single Arrow dataset, split by the time control and piece count.

For the auxiliary scripts: `read_pgn.R` parses big chunks of PGNs into data frames all at once, and has a little helper function to find tablebase positions; `pgn_to_fen.R` uses `python-chess` to turn each PGN into a list of all FENs reached in the game. It is by far the biggest bottleneck, as it's relatively slow and I couldn't find a way to vectorize it: one game runs at a time. I tried to parellelize it, but it let to some bugs. `get_tablebase.R` also uses `python-chess` to take each FEN and check what a locally downloaded tablebase has to say about it.

I discarded games with a rating disparity between the players greater than 200 points, and only stored the average rating of the players. I then collapsed the ratings into bins of 100 points.

## Limitations

- Only used the games played in June 2025;
- Did not use 6 or 7-piece tablebases;
- Data and graph are not colorblind: symmetric positions for black/white are not considered the same, and results are shown with respect to white.
