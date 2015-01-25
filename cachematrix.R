##
## cachematrix.R
## -----------------------------------------------------------------------------
## Coursera Course R Programming: Programming Assignment #2
## -----------------------------------------------------------------------------
## (C) A.Yakovlev, 2015
##

## The function 'makeCacheMatrix' creates a cache for its argument and returns
## a list of functions to access this argument and the cached inversed matrix: 
##  get() - to get the argument matrix itself
##  getInverse() - to get the (cached) inversed matrix of the argument matrix
##  setInverse(t) - to save the matrix t to the cache 

makeCacheMatrix <- function(x = matrix())
{
  cachedData <- NULL
  getInverse <- function() cachedData
  setInverse <- function(inv) cachedData <<- inv
  get <- function() x
  list(get = get, getInverse = getInverse, setInverse = setInverse)
}


## The function 'cacheSolve' calculates the inverse of the matrix, using
## the cached value of the inversed matrix in the case when it has been
## already calculated in the past.

cacheSolve <- function(x, ...)
{
  temp <- x$getInverse()
  if (!is.null(temp))
  {
    message("getting cached data")
    return(temp)
  }
  else
  {
    data <- x$get()
    temp <- solve(data, ...)
    x$setInverse(temp)
    temp
  }
}

