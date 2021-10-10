#' Title Dynamic Programming-Knapsack
#' @param x Data frame consist of two columns w and v
#' @param W Total Capacity
#' @return Algorithm that can solve the knapsack problem exact by iterating over all possible values of w.
#' @export
#'
#' @examples
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#'set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'n <- 2000
#'knapsack_objects <- data.frame(
#' w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#'  dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)

dynamic_knapsack <- function(x, W) {
  if(class(x) != "data.frame") stop ()
  if(all(colnames(x) != c("w", "v"))) stop()
  if(class(W) != "numeric") stop()
  if(W <= 0) stop()

  m <- matrix(data = 0, nrow = nrow(x)+1, ncol = W+1)

  for(i in 1:nrow(x)) {
    for(j in 1:(W+1)) {
      if(x[i,1] > j) {
        m[i+1,j] <- m[i,j]
      }
      else {
        m[i+1,j] <- max(m[i,j], m[i, j-x[i,1]] + x[i,2])
      }
    }
  }

  i <- nrow(m)
  j <- ncol(m)
  elements <- c()

  while(i > 1) {
    if(m[i,j] > m[(i-1),j]) {
      elements[length(elements)+1] <- i-1
      i <- i-1
      j <- j - x[i,1]
    }
    else {
      i <- i-1
    }
  }
  elements <- elements[length(elements):1]

  res <- list(value = round(m[nrow(m),ncol(m)]),
              elements = elements)

  #return(round(m[nrow(m),ncol(m)]))
  return(res)
}

#dynamic_knapsack(x = knapsack_objects[1:16,], W = 3500)
#dynamic_knapsack(x = data.frame(w = c(10,20,30), v = c(60,100,120)), W = 50)

dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
# 16770
dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
# 16770
dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
# 15428
dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)
# 15428

