library(magrittr)
library(stringr)
library(rchess)

pgn <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 d6 { C62 Ruy Lopez: Steinitz Defense } 4. d4 exd4 5. Nxd4 Bd7 6. Nc3 Nxd4 7. Bxd7+ Qxd7 8. Qxd4 Nf6 9. Bf4 Be7 10. O-O-O O-O 11. e5 Nh5 12. Bd2 Qd8 13. g4 c5 14. Qe4 f5 15. Qg2 fxg4 16. Qxg4 g6 17. Rhg1 d5 18. Qxh5 d4 19. Rxg6+ Kh8 20. Rh6 Rf7 21. Qxf7 Qg8 22. Qxe7 dxc3 23. Qf6+ { White wins on time. } 1-0"

pgn_to_fen <- function(pgn) {
    
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
