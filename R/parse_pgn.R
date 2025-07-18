parse_pgn <- function() {
    # Create a connection to read line by line
    con <- file("huge_file.pgn", "r")
    
    read_one_game <- function(con) {
        game_lines <- c()
        
        while(TRUE) {
            line <- readLines(con, n = 1)
            
            # Check if we've reached end of file
            if(length(line) == 0) {
                return(NULL)  # End of file
            }
            
            # Skip empty lines at start
            if(length(game_lines) == 0 && line == "") {
                next
            }
            
            # Add line to current game
            game_lines <- c(game_lines, line)
            
            # Check if we've reached end of game (empty line after moves)
            if(line == "" && length(game_lines) > 1) {
                # Remove the final empty line and return the game
                return(game_lines[-length(game_lines)])
            }
        }
    }
    
    # Usage example:
    game_count <- 0
    while(TRUE) {
        game <- read_one_game(con)
        if(is.null(game)) break  # End of file
        
        game_count <- game_count + 1
        
        # Process the game here
        pgn_text <- paste(game, collapse = " ")
        # ... your processing logic ...
        
        if(game_count %% 1000 == 0) {
            cat("Processed", game_count, "games\n")
        }
    }
    
    close(con)
}