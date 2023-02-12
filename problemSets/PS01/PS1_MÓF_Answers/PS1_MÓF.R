#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

set.seed(123)

?rcauchy()

data <- rcauchy(1000, location = 0, scale = 1)
data


# create empirical distribution of observed data

ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
# D = max(|Fo(x) - Ft(x)|)
D <- max(abs(empiricalCDF - pnorm(data)))
D 

ks.test(data, "pnorm")


# Write an R function that implements the test where the reference distribution
# is normal.

# We want to calculate the p value given the D test-statistic

mMultiply <- function(A, B, C, m){
  i <-0
  j<-0
  while(i<m){
    while(j<m){
      s<-0
      k <-0
      while(k<m){
        s<- s+ A[i*m+k]*B[k*m+j]
        C[i*m+j] <- s
        k<-k+1
      }
      j<-j+1
    }
    i<-i+1
  }
}

mPower <- function(A, eA, V, eV, m, n){
  if(n ==1){
    i<-1
    while(i<m*m){
      V[i] <- A[i]
      eV <-eA
      i<-i+1
    }
    B <- matrix(nrow=m, ncol =m)
    mMultiply(V,V,B,m)
    eB <-2*eV
  }
  if(n%%2==0){
    i <-0
    while(i<m){
      V[i] <- B[i]
      eV <-eB
      i<-i+1
    }
  }
  else{
    mMultiply(A,B,V,m)
    eV <- eA +eB
  }
  if(V[(m/2)*m+(m/2)]>1e140){
    i<-0
    while(i<m){
      V[i]<- V[i]*(1e-140)
      eV <- eV +140
      i<-i+1
    }
  }
  return(B)  
}



knd <- function(n, d) {
  k <- (n*d) +1
  m <- 2*k-1
  h <- k-n*d
  H <- matrix( nrow=m, ncol = m)
  Q <- matrix( nrow=m, ncol = m)
  
  i <-0
  j <- 0
  while(i < m) {
    while(j < m) {
      if(i-j+1 <0){
        H[i*m+j]<-0
      }
      else{
        H[i*m+j] <-1
      }
      j <- j+1
    }
    i <- i+1
  }
  i <-0
  while(i <m){
    H[i*m] <- H[i*m] - (h ^ (i+1))
    H[(m-1)*m+1] <- H[(m-1)*m+1] - (h^(m-i))
    i <- i +1
  }
  H[(m-1)*m] <- H[(m-1)*m] + ifelse(2 * h - 1>0, (2*h-1)^m, 0)
  i<-0
  j<-0
  while(i<m){
    while(j<m){
      if(i-j+1 >0){
        g<-1
        while(g<= i-j+1){
          H[i*m+j] <- H[i*m+j]/g
          g<- g+1
        }
      }
      j <- j+1
    }
    i <- i+1
  }
  eH <-0
  mPower(H, eH, Q, m, n)
  
  s <- Q[(k-1)*m+k-1]
  i<-1
  while(i<=1){
    s<- s*i/n
    if(s<1e-140){
      s<- s*1e140
      eQ <- eQ-140
    }
    i<-i+1
  }
  s<-s*(10^eQ)
  return(s)
  
}


# Expected: small p-value and H0
ks.test(data, "pnorm")
knd(1000,D)



#####################
# Problem 2
#####################

set.seed (123)
data2 <- data.frame(x = runif(200, 1, 10))
data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5)

head(data2)


# Using lm
q2_lm <- lm(y~x, data = data2)
summary(q2_lm)

# Intercept estimate = 0.13919, Slope estimate = 2.72


# Using Newton Raphson method

# We set up the Linear likelihood function

linear.lik <- function(theta, y, X){
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -(0.5)*n*log(2*pi) - (0.5)*n*log(sigma2) - ( (t(e) %*% e)/(2*sigma2))
  return(-logl)
}

linear.MLE <- optim(fn = linear.lik,
                    par = c(1,1,1),
                    hessian = TRUE,
                    y = data2$y,
                    X = cbind(1, data2$x),
                    method = "BFGS"
                    )

linear.MLE$par

# The output is 