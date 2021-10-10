#' Title Greedy Heuristic-Knapsack
#'
#' @param x Data frame consist of two columns w and v
#' @param W Total Capacity
#'
#' @return Algorithm that will not give an exact result,but it will reduce the computational complexity.
#' @export
#'
#' @examples
#'
#' #' RNGversion(min(as.character(getRversion()),"3.5.3"))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <- data.frame(
#' w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#'  greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

greedy_knapsack <- function(x, W) {
  if(class(x) != "data.frame") stop ()
  if(all(colnames(x) != c("w", "v"))) stop()
  if(class(W) != "numeric") stop()
  if(W <= 0) stop()

  x$unit_value <- x[,2] / x[,1]
  x$item <- as.numeric(row.names(x))
  x <- x[order(x$unit_value, decreasing = TRUE),]

  knapsack <- matrix(NA, nrow = 0, ncol = 3)

  for(i in 1:nrow(x)) {
    if((x[i,1] + sum(knapsack[,1])) < W) {
      knapsack <- rbind(knapsack, c(x[i,1], x[i,2], x[i,4]))
    }
    else {
      break
    }
  }
  res <- list(value = round(sum(knapsack[,2])), elements = knapsack[,3])
  return(res)
}
