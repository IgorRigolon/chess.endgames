read_pgn <- function(pgn) {
    
    pgn <- pgn %>%
        as.data.frame()
    
    # marking each game
    
    pgn <- pgn %>%
        setNames("V1") %>%
        mutate(game_id = ifelse(str_detect(V1, "Event"), 1, 0),
               game_id = cumsum(game_id))
    
    # removing incomplete games
    # first and last
    
    pgn <- pgn %>%
        filter(game_id != 0 & game_id != max(game_id))
    
    # remove empty lines
    
    pgn <- pgn %>%
        filter(V1 != "")
    
    # extract all the variables
    
    pgn <- pgn %>%
        mutate(
            name = ifelse(substr(V1, 1, 1) == "[", sub("\\[(\\w+).+", "\\1", V1), "pgn"),
            value = ifelse(substr(V1, 1, 1) == "[", sub("\\[\\w+ \\\"(.+)\\\"\\]", "\\1", V1), V1)
        )
    
    # reshape
    
    pgn <- pgn %>%
        select(-V1) %>%
        tidyr::pivot_wider()
    
    # returning data
    pgn
}

is_tablebase <- function(line) {
    
    piece_count <- line %>%
        str_count("p|r|n|b|q|k|P|R|N|B|Q|K")
    
    piece_count <= 5 & piece_count > 0
}