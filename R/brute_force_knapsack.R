#' Brute_force_Knapsack Function
#'
#' @param x Data frame with two columns
#' @param W knapsack size capacity
#'
#' @return Return best Knapsack combination with maximum value
#' @export
#'
#' @examples
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#'set.seed(42)
#'n <- 2000
#'knapsack_objects <- data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#'  brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#'
brute_force_knapsack= function(x,W){
  stopifnot(is.data.frame(x), apply(x, c(1,2), is.numeric), is.numeric(W), W>=0)
  n=length(x[,1])
  w<-x[,1]
  v<-x[,2]
  result_elements=c()
  result_value=0
  range=1:2^(n)-1
  for(j in range){
    element=which(intToBits(j)==01)
    total_weights=sum(w[element])
    total_value=sum(v[element])
    if(total_value > result_value && total_weights <= W){
      result_elements=element
      result_value=total_value
    }
  }
  result=list("elements"=result_elements,"value"=(result_value))
  return (result)
}

