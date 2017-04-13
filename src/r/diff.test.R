require(data.table)
require(dplyr)

setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")

cv = function (x) {
  x <- x[!is.na(x)] # remove NA values
  
  res <- sd(x) / mean(x)
  return(res)
}

# global variables
tp <- 5 # time points per instance
an <- 6 # number of attributes
cn <- 4 # number of cells / instances

A <- matrix(sample(1:100, an*tp*cn, replace = TRUE), ncol = an)
B <- abs( A - dplyr::lag(A) )
B[seq(1,nrow(B), tp),] <- NA

C <- array(B, dim=c(tp, cn, an))
D <- apply(C, c(2,3), cv)

