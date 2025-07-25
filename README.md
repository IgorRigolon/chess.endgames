# chess.endgames
Evaluate how tablebase positions go in practice

How often do players mess up objectively winning or drawn endgames?. I download 91 million chess games from the [lichess database](https://database.lichess.org/), and detect the ~9 million of them that end up as a tablebase position -- with 7 pieces or fewer in total. I then compare the results that actually happened in-game with the tablebase evaluations, which I obtain from the [lichess API](https://lichess.org/api#tag/Tablebase). I break down the results by rating and time control.

## Overall performance

I only look separately at Rapid, Blitz, and Bullet games, and ignore the specific time controls.

I split players into rating bins of 0-1200, 1200-1600, 1600-2000, and 2000+. 

## Basic checkmates

### Queen vs. King

### Rook vs. King

## King and pawn endgames

### Classical opposition

## Rook and pawn endgames

### Lucena position

### Philidor position

### Vancura position

## Queen and pawn endgames

## Bishop + Knight checkmate

## Two Bishops checkmate

## Troitsky line (Knight vs. King + Pawn)

