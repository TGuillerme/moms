
## Save/load simulation results
save.results <- function(results, path = "../data/processed/") {
    match_call <- match.call()
    save(results, file = paste0(path, as.character(match_call$results), ".Rda"))
}

load.results <- function(results, path = "../data/processed/") {
    load(paste0(path, results, ".Rda"))
    output <- results
    return(output)
}
