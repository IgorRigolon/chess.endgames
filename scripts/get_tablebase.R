get_tablebase_eval <- function(fen) {
    
    board <- chess$Board(fen)
    
    # get tablebase WDL
    
    tablebase$probe_wdl(board)
    
}