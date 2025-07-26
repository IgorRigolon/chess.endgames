get_tablebase_eval <- function(fen) {
    cmd <- sprintf('wsl fathom --path=/mnt/d/Tablebase "%s"', fen)
    result <- system(cmd, intern = TRUE)
    
    result <- paste(result, collapse = "\n")
    
    # Extract WDL value
    result %>%
        stringr::str_extract('(?<=\\[WDL ")[^"]+')
}