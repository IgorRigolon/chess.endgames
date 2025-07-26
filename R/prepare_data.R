# aggregate into rating bins, time controls and types of positions
# types of positions: material imbalances and theoretical endgames

library(arrow)
library(dplyr)

dat <- data.table::fread(
    "data/tablebase_positions.csv", 
    col.names = c(
        "id", "fens", "event", "white_elo", "black_elo",
        "result", "opening", "time_control", "termination"
    )
)

# add tablebase evaluations

dat <- dat %>%
    mutate(
        eval = query_tablebase(fen)
    )

# remove large rating discrepancies

dat <- dat %>%
    filter(abs(white_elo - black_elo) < 200)

# calculate average rating of the two players

dat <- dat %>%
    mutate(avg_elo = (white_elo + black_elo)/2) %>%
    mutate(avg_elo = 100 * as.integer(avg_elo/100) + 50)

# collapse time controls into rapid, blitz, and bullet

# keep only some columns

dat <- dat %>%
    select(
        id, eval, result, time_control, fens, avg_elo, termination
    )

# save as parquet

dat %>%
    group_by(time_control, avg_elo) %>%
    arrow::write_dataset("data")