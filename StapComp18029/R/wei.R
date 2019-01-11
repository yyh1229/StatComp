#' @title Calculate the correlation between A with each dimension of B.
#' @description Using the function "cor()" to measure the connectinity of each dimension of B with A.
#' @param X the firt variable in arbitrary dimension.
#' @param Y the second varai, which must be multidimensional.
#' @import energy
#' @examples
#' \dontrun{
#' X <- rnorm(10)
#' Y <- matrix(rnorm(10*5),10,5)
#' wei(X,Y)
#' }
#' @export
wei <- function(X,Y){
  w <- apply(Y,2,cor,X)
  return(w)
}
