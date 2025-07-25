moves_to_fen <- function(moves) {
    
    # split into individual moves
    moves <- moves %>%
        str_split(" ", simplify = TRUE) %>%
        str_trim()
    
    # remove empty moves
    moves <- moves[moves != ""]
    
    # create rchess game to follow along
    game <- Chess$new()
    
    # get vector with FENs
    
    purrr::map_chr(
        moves,
        function(move) {
            game$move(move)
            
            game$fen()
        }
    )
}

pgn_to_moves <- function(pgn) {
    # first, remove all the text between brackets
    pgn <- pgn %>%
        str_remove_all("\\{[^}]*\\}")
    
    # remove the result at the end
    pgn <- pgn %>%
        str_remove_all("\\s+[^\\s]*$")
    
    # remove all move markers
    moves <- pgn %>%
        str_remove_all("\\d+\\.") %>%
        str_remove_all("\\.")
    
    # remove annotations
    moves <- moves %>%
        str_remove_all("[!?]") %>%
        str_trim()
    
    moves
}