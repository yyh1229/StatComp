#' @title Select adjacent parts from Y as a group with the stongest connection with X.
#' @describeIn  Select adjacent parts from Y as a group with the stongest connection with X, the connection is measured by "dcor()".
#' @param k the size of the group
#' @param X the firt variable in arbitrary dimension.
#' @param Y the second varaiable, which must be multidimensional.
#' @import energy
#' @examples
#' \dontrun{
#' X <- rnorm(10)
#' Y <- matrix(rnorm(10*20),10,20)
#' select.group(X,Y,k=3)
#' }
#' @export
select.group <-function(X,Y,k){
  a <- NULL
  p <- dim(Y)[2]
  for(i in 1:(p-k+1)){
    Y1 <- Y[,i:(i+k-1)]
    W <-apply(Y1,2,dcor,X)
    wy <- Y1%*%diag(W)
    a1 <- dcor(X,wy)
    a <- c(a,a1)
  }
  index <-which.max(a)
  group <-Y[,index:(index+k-1)]
  w <- apply(group,2,dcor,X)
  return(list(Y.group=group, column=c(index:(index+k-1)),cor=w))
}


