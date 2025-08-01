# chess.endgames
An interactive graph to see how tablebase positions play out in practice

Click on the image below to open the data visualization. You can filter games by time control, rating range, and by either material count or a specific FEN. Tablebase evaluations and results are always shown from white's perspective. _Note: It runs locally on your browser, so it may slow down your computer._

[Click here to see]

Even though chess positions with 7 pieces or fewer are solved by computers, they're not always simple for humans to play correctly. I analyze ~91 million games played on [lichess](lichess.org) to answer the question:

> How often do players mess up objectively winning or drawn endgames?

## General statistics

Out of the 91,...,... games, [x] of them (y%) reached a tablebase position. That amounts to [x] tablebase FENs, out of which [y] are unique.

## Examples

## Workflow

I manually downloaded the games played in June 2025 from the [lichess database](https://database.lichess.org/) and did three things with them: (i) I parsed the PGNs into data frames, (ii) I generated all the FENs reached in the games and checked which are tablebase positions, and (iii) I probed a locally-downloaded 3-4-5 piece [Syzygy tablebase](https://syzygy-tables.info/#download) to get the tablebase evaluations of positions. Even though I did everything in R, these last two steps relied heavily on the [`python-chess` library](https://pypi.org/project/chess/).

The `scripts/process_database.R` script receives a big lichess database file and spits out the tablebase positions found within them, along with the ratings of players and the actual result of the games. The other scripts are auxiliary ones called in the beginning of `process_database.R`. The output of this script is stored in `/data-raw/`, where each chunk of games is stored in a separate `.parquet` file. Once all chunks are done processing, they're written into a single Arrow dataset, split by the time control and piece count.

## Limitations

- Only used the games played in June 2025;
- Did not use 6 or 7-piece tablebases;
- Data and graph are not colorblind: symmetric positions for black/white are not considered the same, and results are shown with respect to white.
