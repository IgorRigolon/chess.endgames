base_url <- "https://tablebase.lichess.ovh"

h <- new_handle()

handle_setopt(h, httpheader = c("Accept" = "application/json"))

query_tablebase <- function(fen) {
    
    
    
    # replacing spaces by underscores in fen
    fen <- gsub(" ", "_", fen)
    
    # full url
    url <- paste0(base_url, "/", "standard", "?fen=", fen)
    
    response <- curl_fetch_memory(url, handle = h)
    
    fromJSON(rawToChar(response$content))$category
}