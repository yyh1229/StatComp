#' @title Calculate the the weighted dcor
#' @description Calculate the new dcor after paying different weight to every dimension of B.
#' @param w the weight of each dimension of B
#' @param X the firt variable in arbitrary dimension.
#' @param Y the second varaiable, which must be multidimensional.#'
#' @import energy
#' @examples
#' \dontrun{
#' X <- rnorm(10)
#' Y <- matrix(rnorm(10*5),10,5)
#' W<- runif(5,0,1)
#' newdcor(X,Y,w=W)
#' }
#' @export
newdcor <- function(X,Y,w){
  wy <- Y%*%diag(w)
  new <-dcor(X,wy)
  return(new)
}
