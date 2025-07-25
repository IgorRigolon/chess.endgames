library(stringr)
library(jsonlite)
library(bigchess)
library(dplyr)
library(rchess)
library(furrr)
library(curl)
library(reticulate)

plan(multisession, workers = 2)

source("R/pgn_to_fen.R")
source("R/read_pgn.R")

pgntofen <- import("pgntofen")

# Create the converter object
pgnConverter <- pgntofen$PgnToFen()

pgnConverter$pgnToFen(c("d4", "d5"))
fen <- pgnConverter$getFullFen()

print(fen)

con <- file("D:/lichess_db_standard_rated_2025-06.pgn", "r")

batch_size <- 1e6

past_rows <- 0

past_endgames <- 0

while(length(dat <- readLines(con, n = batch_size)) > 0) {
    
    # parse PGNs into a data frame
    dat <- suppressWarnings(read_pgn(dat))
    
    # extract only individual moves from FENs
    dat <- dat %>%
        mutate(pgn = pgn_to_moves(pgn)) %>%
        mutate(id = row_number() + past_rows)
    
    # recording games already read
    
    past_rows <- past_rows + nrow(dat)
    
    # count captures
    dat <- dat %>%
        mutate(
            capture_count = str_count(pgn, "x")
        )
    
    # keep only games with enough captures to reach tablebase
    dat <- dat %>%
        filter(capture_count >= 25)
    
    past_endgames <- past_endgames + nrow(dat)
    
    # go over each game and build FENs
    
    dat <- dat %>%
        mutate(
            fens = furrr::future_map(
                pgn,
                function(moves) {
                fen <- moves_to_fen(moves)
                fen[is_tablebase(fen)]
            }, .progress = TRUE)
        ) %>%
        tidyr::unnest(fens) %>%
        select(id, event = Event, white_elo = WhiteElo, black_elo = BlackElo,
               result = Result, opening = Opening, time_control = TimeControl,
               termination = Termination, fens)
    
    readr::write_csv(dat, "data/tablebase_positions.csv", append = TRUE)
    
    message(paste(past_rows, "games -", past_endgames, "endgames"))
}