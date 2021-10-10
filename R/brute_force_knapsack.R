library(parallel)


#' Title Brute Force-Knapsack
#' @param x data frame consist of two columns w and v
#' @param W total capacity
#' @param parallel To Support for parallel computation
#' @import parallel
#' @return Algorithm that goes through all possible alternatives and return the maximum value found
#' @export
#'
#' @examples
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <- data.frame(
#' w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#'  brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack <- function(x, W, parallel = FALSE) {
  if(class(x) != "data.frame") stop ()
  if(all(colnames(x) != c("w", "v"))) stop()
  if(class(W) != "numeric") stop()
  if(W <= 0) stop()

  res <- list()
  res[["value"]] <- 0

  if(parallel == TRUE) {
    cores <- detectCores()
    cl <- makeCluster(cores, type = "PSOCK")
    clusterExport(cl, c("x", "W", "res"), envir = environment())

    binrep <- parSapply(cl, (1:(2^nrow(x)-1)), function(i) {
      sum(x[which(intToBits(i) == 1), 1])
    })

    binrep_sums <- parSapply(cl, (1:(2^nrow(x)-1))[which(binrep <= W)], function(i) {
      sum(x[which(intToBits(i) == 1), 2])
    })

    res[["value"]] <- round(max(binrep_sums))
    res[["elements"]] <- which(intToBits((1:(2^nrow(x)-1))[which(binrep <= W)][which.max(binrep_sums)]) == 1)

    stopCluster(cl)
  }

  else {
    for(i in 1:(2^nrow(x)-1)) {
      if( sum(x[which(intToBits(i) == 1), 1]) <= W & sum(x[which(intToBits(i) == 1), 2]) > res[["value"]] ) {
        res[["value"]] <- round(sum(x[which(intToBits(i) == 1), 2]))
        res[["elements"]] <- which(intToBits(i) == 1)
      }
    }
  }

  return(res)
}
