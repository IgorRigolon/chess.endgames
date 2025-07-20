library(magrittr)
library(stringr)
library(jsonlite)

source("R/pgn_to_fen.R")

con <- file("D:/lichess_db_standard_rated_2025-06.pgn", "r")

is_tablebase <- function(line) {
    piece_count <- line %>%
        str_count("p|r|n|b|q|k|P|R|N|B|Q|K")
    
    piece_count <= 8
}

is_pgn <- function(line) substr(line, 1, 2) == "1."

counter <- 0

batch_size <- 1

while (length(line <- readLines(con, n = batch_size)) > 0) {
    
    counter <- counter + 1
    
    message(counter * batch_size)
    
    # keep only the PGN itself
    
    line <- line[is_pgn(line)]
    
    # for each game, get sequence of FENs and keep if they're tablebase positions
    
    purrr::imap(
        line,
        function(pgn, i) {
            
            result <- stringr::str_sub(pgn, -3)
            
            fens <- pgn_to_fen(pgn)
            
            fens <- fens[is_tablebase(fens)]
            
            games <- tibble::tibble(
                "id" = i,
                "fens" = fens,
                "result" = result
            )
            
            readr::write_csv(games, "data/tablebase_positions.csv", append = TRUE)
        }
    )
}
