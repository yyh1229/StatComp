#' @title Select the top k'th property of Y as a group with the stongest connection with X
#' @describeIn  Select a group of Y which consisting of the top k'th strongest connectivity parts with X.
#' @param k the size of the group
#' @param X the firt variable in arbitrary dimension.
#' @param Y the second varai, which must be multidimensional.
#' @import energy
#' @examples
#' \dontrun{
#' X <- rnorm(10)
#' Y <- matrix(rnorm(10*20),10,20)
#' select.top(X,Y,k=3)
#' }
#' @export
select.top <-function(X,Y,k){
  p <- dim(Y)[2]
  w <- apply(Y,2,dcor,X)
  w.top <- sort(w,decreasing = TRUE)[1:k]
  index <- NULL
  for(i in 1:k) {
    index <- c(index, which(w==w.top[i]))
  }
  Y.top <- Y[,index]
  return(list(Y.top=Y.top, column=index, cor=w[index]))
}


