#' @title Independence test using "dcor"
#' @describeIn  Using "dcor" to perform a statistical test of dependence with a permutation test.
#' @param x data of the first sample
#' @param y data of the second sample
#' @param R number of replicates
#' @import energy
#' @examples
#' \dontrun{
#' x <- rnorm(10)
#' y <- matrix(rnorm(10*5),10,5)
#' Dcor.test(x,y,R=199)
#' }
#' @export
Dcor.test <- function(x,y,R){
  x <- as.matrix(x)
  y <- as.matrix(y)
  n <- dim(x)[1]
  T0 <- dcor(x,y)
  T1 <- rep(0,R)
  for(i in 1:R){
    x1 <- x[sample(n)]
    T1[i] <- dcor(x1,y)
  }
   T <- c(T0,T1)
   p_value <- mean(T >= T0)
   return(list(statistic=T0, p.value=p_value,replicates= R))
}
