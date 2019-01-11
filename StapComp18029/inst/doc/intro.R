## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
sample(0:1,size=10,replace=TRUE)

## ------------------------------------------------------------------------
sample(1:100,size=6,replace=FALSE)

## ------------------------------------------------------------------------
sample(letters,size=26)  # sample(letters,size=length(letters))

## ------------------------------------------------------------------------
x <- sample(1:3,size=100,replace=TRUE,prob=c(0.2,0.3,0.5))
x
table(x)

## ------------------------------------------------------------------------
n <- 1000
u <- runif(n)
x <- u^(1/3)
hist(x,prob = TRUE,main=expression(f(x)==3*x^2))  #density histogram of sample
y <- seq(0,1,0.01)
lines(y,3*y^2)  #density curve f(x)

## ------------------------------------------------------------------------
names(iris)
table(iris$Species)
w <- iris[[2]]  # Sepal.Width
y <- subset(iris, Species == "versicolor",select = Petal.Length)
summary(y)

## ------------------------------------------------------------------------
f <- function(x,y) {
z <- (1/(2*pi)) * exp(-.5 * (x^2 + y^2)) }
y <- x <- seq(-3, 3, length= 50)
z <- outer(x, y, f) #compute density for all (x,y)
persp(x, y, z, theta = 45, phi = 30, expand = 0.6, ltheta = 120, shade = 0.75, ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "f(x, y)",col="lightblue")

## ------------------------------------------------------------------------
set.seed(1234)
n <- 1000
u <- runif(n)
x <- c(0,1,2,3,4)
p <- c(0.1,0.2,0.2,0.2,0.3)
cp <- cumsum(p)
x.sample <- x[findInterval(u,cp)+1]
table(x.sample)/n # calculate the frequency

## ------------------------------------------------------------------------
set.seed(12345)
x.sample2 <- sample(x,size=n,replace=TRUE,prob=p)
table(x.sample2)/n

## ------------------------------------------------------------------------
n <- 1000
x <- numeric(n)
i <- 0
while( i<=n ){
  u <- runif(1)
  y <- runif(1)  # random variate from g
  if (u < y^2*(1-y)){
    x[i] <-y 
    i <- i+1
  }
}

hist(x,breaks=seq(0,1,0.1),prob=TRUE,main=expression(f(x)==12*x^2*(1-x)))
z <- seq(0,1,0.01)
lines(z,12*z^2*(1-z))

## ------------------------------------------------------------------------
n <- 1000
r <- 4
beta <- 2
lambda <- rgamma(n,r,beta)
y <- rexp(n,lambda)
y

## ------------------------------------------------------------------------
set.seed(1234)
x <- seq(0.1,0.9,0.1)
f <- function(x) 1/beta(3,3)*x^2*(1-x)^2  # density function

for(i in 1:9){
  U <- runif(10000,min=0,max=x[i])
  F[i] <- mean(x[i]*f(U))  # CDF
}
round(F,5)
pbeta(x,3,3)

plot(F,col=2)
points(pbeta(x,3,3),col=4,pch=3)

## ------------------------------------------------------------------------
# We set the value of sigma to 1,4,20 respectively.
set.seed(123456)
n <- 1000000
sigma <- c(1,4,20,200)
for(i in 1:4){
  u1 <- runif(n/2)
  u2 <- runif(n/2)
  T1 <- 0.5*(sqrt(-2*sigma[i]^2*log(1-u1))+sqrt(-2*sigma[i]^2*log(1-u2))) # independent X1 and X2 
  T2 <- 0.5*(sqrt(-2*sigma[i]^2*log(1-u1))+sqrt(-2*sigma[i]^2*log(u1)))  # negative correlation fo X and X'
  V1 <- var(T1)
  V2 <- var(T2)
  print((V1-V2)/V1*100)  # percent reduction in variance
}

## ------------------------------------------------------------------------
x <- seq(1,10,0.01)
g <- x^2/sqrt(2*pi)*exp(-x^2/2)
f1 <- 2/pi/(1+(x-1.5)^2)
f2 <- 0.5
plot(x,g,type="l",ylim=c(-0.1,0.8))
points(x,f1,type="l",col=4)
points(x,rep(f2,length(x)),type="l",col=6)
legend(8,0.2,c("g","f1","f2"),col=c(1,4,6),lty=c(1,1,1))

## ------------------------------------------------------------------------
plot(x,g/f1,type="l",col=2,ylim=c(-1,1))
points(x,g/f2,type="l",col=3)
legend(8,1,c("g/f1","g/f2"),col=c(2,3),lty=c(1,1))

## ------------------------------------------------------------------------
set.seed(5679)
n <- 100000
x <- rcauchy(n,location=1.5,scale=1)
g <- x^2/sqrt(2*pi)*exp(-x^2/2)
f1 <- 2/pi/(1+(x-1.5)^2)
theta.hat <- mean(g/f1)
round(theta.hat,3)

## ------------------------------------------------------------------------
m <- 1000;n <- 100;
set.seed(1234)
G <- numeric(m)
for(i in 1:m){
  x <- rlnorm(n)   # standard lognormal
  x <- sort(x)
  mu.hat <- mean(x)
  G[i] <- 1/n^2/mu.hat*sum((2*(1:n)-n-1)*x)
}
mean(G)
median(G)
quantile(G,seq(0.1,0.9,0.1))

## ------------------------------------------------------------------------
m <- 1000;n <- 100;
set.seed(1234)
G1 <-G2 <- numeric(m)
for(i in 1:m){
  x1 <- runif(n)  # uniform distribution
  x1 <- sort(x1)
  x2 <- rbinom(n,1,0.1)  # Bernoulli(0.1)
  x2 <- sort(x2)
  mu.hat1 <- mean(x1)
  mu.hat2 <- mean(x2)
  G1[i] <- 1/n^2/mu.hat1*sum((2*(1:n)-n-1)*x1)
  G2[i] <- 1/n^2/mu.hat2*sum((2*(1:n)-n-1)*x2)

}
print(c(mean(G1),median(G1)))  ## uniform distribution
print(c(mean(G2),median(G2)))  ## Bernoulli(0.1)
quantile(G1,seq(0.1,0.9,0.1))  ## uniform distribution
quantile(G2,seq(0.1,0.9,0.1))  ## Bernoulli(0.1)

# PLOT
hist(G,xlim=c(0,1),ylim=c(0,15),freq=FALSE)
hist(G1,xlim=c(0,1),ylim=c(0,15),freq=FALSE)
hist(G2,xlim=c(0,1),ylim=c(0,15),freq=FALSE)

## ------------------------------------------------------------------------
# function to calculate the confidence interval
confidence.interval <- function(m,n,a,b,alpha){
  G <- numeric(m)
  for(i in 1:m){
    x <- rlnorm(n,meanlog=a,sdlog=b)
    x <- sort(x)
    mu.hat <- mean(x)
    G[i] <- 1/n^2/mu.hat*sum((2*(1:n)-n-1)*x)
  }
  G.hat <- mean(G)
  G.sd <- sd(G)
  g1 <-G.hat-qnorm(1-alpha/2)*G.sd
  g2 <-G.hat+qnorm(1-alpha/2)*G.sd
  return(c(g1=g1,g2=g2))
}

m <- 100;n <- 100;N <-1000;n0 <- 100;alpha=0.05
set.seed(123)
p <- numeric(N)
for(i in 1:N){
  a <- sample(1:10,size=1,replace=TRUE)
  b <- 0.1
  g <-confidence.interval(m,n,a=a,b=b,alpha=alpha) 
  mu <- exp(a+0.5*b^2) 
  x <- rlnorm(n0,meanlog=a,sdlog=b)
  x<-sort(x)
  G0 <- 1/n0^2/mu*sum((2*(1:n0)-n0-1)*x)  # the real value of G

  if(G0<=g[2] && G0>=g[1]){
    p[i] <- 1
  }else{
    p[i] <- 0
  }
}
mean(p)

## ------------------------------------------------------------------------
require(mvtnorm)
set.seed(123)
m <- 500 # Number of Monte Carlo trials
n <- 100
alpha <- 0.05
c <- 2
df <- 4

tmp <- matrix(rnorm(c^2), c, c)
mcov <- tcrossprod(tmp, tmp) + diag(0.5, c)
power_rho <- power_rhos <- power_tau <- numeric(m)
for(i in 1:m){
  tmp <- matrix(rnorm(c^2), c, c)
  mcov <- tcrossprod(tmp, tmp) + diag(0.5, c)
  bvtn <- rmvt(n,sigma=mcov, df = df,delta=1:2) 
  x <- bvtn[,1]
  y = bvtn[,2]
  power_rho[i] <- cor.test(x,y,method="pearson")$p.value<0.05
  power_rhos[i] <- cor.test(x,y,method="kendall")$p.value<0.05
  power_tau[i] <- cor.test(x,y,method="spearman")$p.value<0.05
}
print(c(mean(power_rho),mean(power_rhos),mean(power_tau)),3)

## ------------------------------------------------------------------------
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

## ------------------------------------------------------------------------
lf1 <- vector("list", length(formulas))
for (i in seq_along(formulas)){
  lf1[[i]] <- lm(formulas[[i]], data = mtcars)
}
lf1

## ------------------------------------------------------------------------
# with two version

la1 <- lapply(formulas, lm, data = mtcars)
la2 <- lapply(formulas, function(x) lm(formula = x, data = mtcars))
c(la1,la2)

## ------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i){
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})#eturns a list of the same length as mtcars

## ------------------------------------------------------------------------
## for loop
lf <- vector("list", length(bootstraps))
for (i in seq_along(bootstraps)){
  lf[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}
lf

## ------------------------------------------------------------------------
# lapply without anonymous function
la <- lapply(bootstraps, lm, formula = mpg ~ disp)
la

## ------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

## ------------------------------------------------------------------------
sapply(la1, rsq)
sapply(la2, rsq)
sapply(lf1, rsq)

## ------------------------------------------------------------------------
sapply(la, rsq)
sapply(lf, rsq)

## ------------------------------------------------------------------------
trials <- replicate(100,t.test(rpois(10, 10), rpois(7, 10)),
                    simplify = FALSE)

## ------------------------------------------------------------------------
# anonymous function:
sapply(trials, function(test) test$p.value)
sapply(trials, '[[', i = "p.value") 

## ------------------------------------------------------------------------
library(parallel)
testlist <- list(iris, mtcars, cars)
lapply(testlist, function(x) vapply(x, mean, numeric(1)))

## ------------------------------------------------------------------------
lmapply <- function(X, FUN, FUN.VALUE, simplify = FALSE){
  out <- Map(function(x) vapply(x, FUN, FUN.VALUE), X)
  if(simplify == TRUE){return(simplify2array(out))}
  out
}
lmapply(testlist, mean, numeric(1))

## ------------------------------------------------------------------------
set.seed(1)
# function : Cramer-von Mises statistic
W2 <- function(x,y){
  Fn <- ecdf(x) # ecdf function of f(x)
  Gn <- ecdf(y) # ecdf function of g(x)
  n <-length(x)
  m <- length(y)
 out <-  (sum(( Fn(x) - Gn(x))^2) + sum(( Fn(y) - Gn(y) )^2))*m*n/((m+n)^2)
 return(out)
}


# Example 8.1
data("chickwts")
attach(chickwts)
x <- as.vector(weight[feed == "soybean"])
y <- as.vector(weight[feed == "linseed"]) 
z <- c(x,y)
n <- length(x)
m <- length(y)

R <- 999
T.star <- numeric(R)
T0 <- W2(x,y)

# permutation 
for(i in 1:R){
  z.star<- sample(z,m+n,replace=FALSE)
  x.star <- z.star[1:n]
  y.star <- z.star[(n+1): (m+n)]
  T.star[i] <- W2(x.star,y.star)
}

Tb <- c(T0,T.star)
p.value <-mean(T0 <= Tb)
print(p.value) # Exercise 8.1

## ------------------------------------------------------------------------
hist(T.star,density = FALSE,xlab="W2",ylab="Frequency",main="Histogram of W2 in Example 8.1")
abline(v=T0,col="red",lty=1,lwd=3)

## ------------------------------------------------------------------------
# Example 8.2
x <- as.vector(weight[feed == "sunflower"])
y <- as.vector(weight[feed == "linseed"])
z <- c(x,y)
n <- length(x)
m <- length(y)

R <- 999
T.star <- numeric(R)
T0 <- W2(x,y)

# permutation 
for(i in 1:R){
  z.star<- sample(z,m+n,replace=FALSE)
  x.star <- z.star[1:n]
  y.star <- z.star[(n+1): (m+n)]
  T.star[i] <- W2(x.star,y.star)
}

Tb <- c(T0,T.star)
p.value <-mean(T0 <= Tb)
print(p.value) # Exercise 8.2

## ------------------------------------------------------------------------
hist(T.star,density = FALSE,xlab="W2",ylab="Frequency",main="Histogram of W2 in Example 8.2",xlim=c(0,2))
abline(v=T0,col="red",lty=1,lwd=3)

## ------------------------------------------------------------------------
set.seed(1)
# density function of Cauchy(0,1)
f <- function(x,theta=1,eta=0){
  out <- 1/(pi * theta * (1+((x-eta)/theta)^2))
  return(out)
}
m <- 100000
x <- numeric(m)
u <- runif(m)
sigma <- 1
x[1] <- runif(1,-40,40)
k <- 0  # record rejection times

for(i in 2:m){
  xt <- x[i-1]
  y <- rnorm(1, mean=xt, sd=sigma)
  num <- f(y) * dnorm(xt,mean=y,sd=sigma)
  den <- f(xt) * dnorm(y,mean=xt,sd=sigma)
  if(u[i] <= min(num/den,1)) {
    x[i] <- y
  }else{
    x[i] <- xt
    k <- k+1
  }
}


# rejection rate
print(k/m)


# plot the sample vs the time index.
b <- 1001
rest <- b:m
par(mfrow=c(1,2))
plot(rest,x[rest],type="l")
hist(x[rest],probability=TRUE,breaks=100)
par(mfrow=c(1,1))

# compare the decilies
Q.obser <- quantile(x[rest],seq(0,1,0.1))
Q.expec <- qcauchy(seq(0,1,0.1))
print(round(cbind(Q.obser , Q.expec ), 4)) 

#qqplot
x1 <- x[rest]
a <- ppoints(1000)
QR <- qcauchy(a)
Q <- quantile(x1,a)
qqplot(QR,Q,main="",xlab="Rayleigh Quantiles",ylab="Sample Quantiles",xlim=c(-20,20),ylim=c(-20,20))

#qqline(obser, datax = FALSE, distribution = qcauchy,
       #probs = c(0.1, 0.9), qtype = 7)

## ------------------------------------------------------------------------
# function 
f <- function(x,N){
  if(x<0 || x>1){
    return(0)
  }else{
  out <- (0.5+0.25*x)^N[1] * ((1-x)/4)^N[2] * ((1-x)/4)^N[3] * (x/4)^N[4]
  return(out)
  }
}

Num <- c(125,18,20,34)  ##  group sizes of 4  categories
m <- 5000   ## length of the chain
u <- runif(m)   ## for accept/reject step
burn <- 1000    ## burn-in time
theta <- numeric(m)  ## the chain
width <- 0.2
x[1] <- 0.5 
k <- 0

for(i in 2:m){
  xt <- x[i-1]
  y <- x[i-1] + runif(1,-width,width)
  alpha <- min(1,f(y,N=Num)/f(xt,Num))
  if(alpha >= u[i]){
    x[i] <- y
  }else{
    x[i] <- x[i-1]
    k <- k+1
  }
}

# PLOT
index <- burn:m
x1 <- x[index]
par(mfrow=c(1,2))
plot(index,x1,type="l",xlab="",ylab="theta")
hist(x[index])
abline(v=mean(x[index]),col="red")
print(mean(x[index])) # the estimate of theta

