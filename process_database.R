library(stringr)
library(jsonlite)
library(bigchess)
library(dplyr)
library(rchess)
library(purrr)
library(curl)
library(reticulate)
library(arrow)

source("scripts/pgn_to_fen.R")
source("scripts/read_pgn.R")
source("scripts/get_tablebase.R")

chess <- import("chess")
chess.pgn <- import("chess.pgn")
chess.syzygy <- import("chess.syzygy")

tablebase <- chess.syzygy$open_tablebase("D:/Tablebase")

# connecting to massive database

con <- file("D:/lichess_db_standard_rated_2025-06.pgn", "r")

batch_size <- 1e6

past_rows <- 0

past_endgames <- 0

chunk <- 0

skip_to_chunk <- 1382

while (length(dat <- readLines(con, n = batch_size)) > 0) {
  chunk <- chunk + 1

  if (chunk < skip_to_chunk) {
    message(chunk)
    next
  }

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

  # go over each game and build FENs

  dat <- dat %>%
    mutate(
      fens = purrr::map(
        pgn,
        function(moves) {
          fen <- moves_to_fen(moves)
          fen[is_tablebase(fen)]
        },
        .progress = "Generating FENs for each game"
      )
    ) %>%
    tidyr::unnest(fens) %>%
    select(id,
      event = Event, white_elo = WhiteElo, black_elo = BlackElo,
      result = Result, termination = Termination, fens
    )

  # get the tablebase evaluations

  dat <- dat %>%
    mutate(
      eval = purrr::map_int(
        fens, get_tablebase_eval,
        .progress = "Probing tablebase evaluations"
      )
    )

  # tablebase evals in terms of white/black

  dat <- dat %>%
    mutate(
      white_to_move = str_detect(fens, " w "),
      eval = eval * (2 * white_to_move - 1),
      result = case_match(
        result,
        "1-0" ~ 1,
        "1/2-1/2" ~ 0,
        "0-1" ~ -1
      )
    )

  # some extra data cleaning
  # remove large rating discrepancies

  dat <- dat %>%
    mutate(across(c("black_elo", "white_elo"), as.numeric)) %>%
    filter(abs(white_elo - black_elo) < 200)

  # calculate average rating of the two players

  dat <- dat %>%
    mutate(avg_elo = (white_elo + black_elo) / 2) %>%
    mutate(avg_elo = 100 * as.integer(avg_elo / 100) + 50)

  # getting rid of the original rating variables

  dat <- dat %>%
    select(
      -c(white_elo, black_elo)
    )

  # extracting time control

  dat <- dat %>%
    mutate(
      time_control = stringr::str_extract(event, "UltraBullet|Bullet|Blitz|Rapid|Classical")
    ) %>%
    filter(
      time_control %in% c(
        "UltraBullet", "Bullet", "Blitz", "Rapid", "Classical"
      )
    )

  # remove event variable

  dat <- dat %>%
    select(-event)

  past_endgames <- past_endgames + nrow(dat)

  # saving each batch to a .parquet

  write_parquet(
    dat,
    paste0("data-raw/", "batch_", chunk, ".parquet")
  )

  message(paste0("Chunk ", chunk, ": ", past_rows, " games - ", past_endgames, " endgames "))
}

# opening parquet chunks

dat <- open_dataset("data-raw")

# counting material

dat <- dat %>%
  mutate(
    white_queen = str_count(fens, "Q"),
    black_queen = str_count(fens, "q"),
    white_rook = str_count(fens, "R"),
    black_rook = str_count(fens, "r"),
    white_knight = str_count(fens, "N"),
    black_knight = str_count(fens, "n"),
    white_bishop = str_count(fens, "B"),
    black_bishop = str_count(fens, "b"),
    white_pawn = str_count(fens, "P"),
    black_pawn = str_count(fens, "p"),
    white_material = white_queen + white_rook +
        white_knight + white_bishop + white_pawn,
    black_material = black_queen + black_rook +
      black_knight + black_bishop + black_pawn
  ) %>%
  filter(white_material + black_material > 0, !is.na(result))

# splitting into parquet chunks

dat %>%
  group_by(time_control, white_material, black_material) %>%
  arrow::write_dataset("data", max_rows_per_file = 3000000)