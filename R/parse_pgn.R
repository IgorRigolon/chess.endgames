library(stringr)
library(jsonlite)
library(bigchess)
library(dplyr)
library(rchess)
library(purrr)
library(curl)
library(reticulate)
library(arrow)
library(furrr)

plan(multisession, workers = 2)

source("R/pgn_to_fen.R")
source("R/read_pgn.R")
source("R/get_tablebase.R")

chess <- import("chess")
chess.pgn <- import("chess.pgn")
chess.syzigy <- import("chess.syzygy")

# connecting to massive database

con <- file("D:/lichess_db_standard_rated_2025-06.pgn", "r")

batch_size <- 1e6

past_rows <- 0

past_endgames <- 0

chunk <- 0

while(length(dat <- readLines(con, n = batch_size)) > 0) {
    
    chunk <- chunk + 1
    
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
        filter(capture_count >= 27)
    
    past_endgames <- past_endgames + nrow(dat)
    
    # go over each game and build FENs
    
    dat <- dat %>%
        mutate(
            fens = purrr::map(
                pgn,
                function(moves) {
                fen <- moves_to_fen(moves)
                fen[is_tablebase(fen)]
            }, .progress = "Generating FENs for each game")
        ) %>%
        tidyr::unnest(fens) %>%
        select(id, event = Event, white_elo = WhiteElo, black_elo = BlackElo,
               result = Result, termination = Termination, fens)
    
    # get the tablebase evaluations
    
    dat <- dat %>%
        mutate(
            eval = furrr::future_map(
                fens, get_tablebase_eval,
                .progress = TRUE
            )
        )
    
    # some extra data cleaning
    # remove large rating discrepancies
    
    dat <- dat %>%
        mutate(across(c("black_elo", "white_elo"), as.numeric)) %>%
        filter(abs(white_elo - black_elo) < 200)
    
    # calculate average rating of the two players
    
    dat <- dat %>%
        mutate(avg_elo = (white_elo + black_elo)/2) %>%
        mutate(avg_elo = 100 * as.integer(avg_elo/100) + 50)
    
    # getting rid of the original rating variables
    
    dat <- dat %>%
        select(
            - c(white_elo, black_elo)
        )
    
    # extracting time control
    
    dat <- dat %>%
        mutate(
            time_control = stringr::str_extract(event, "UltraBullet|Bullet|Blitz|Rapid|Classical")
        )
    
    # remove event variable
    
    dat <- dat %>%
        select(-event)
    
    # saving to parquet
    
    dat %>%
        group_by(time_control, avg_elo) %>%
        arrow::write_dataset("data")
    
    message(paste0("Chunk ", chunk, ": ", past_rows, " games - ", past_endgames, " endgames "))
}