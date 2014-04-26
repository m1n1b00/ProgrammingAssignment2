## This file contains two functions for caching the mean of a matrix x. 
## makeCacheMatrix caches the inverse of matrix x. The return value of 
## makeCacheMatrix can be used as input to the function cacheSolve which will 
## either retrieve the inverse of x if cached or compute the inverse of x and
## then cache the results. Once x is initialized within makeCacheMatrix, it 
## will have getter and setter functions accessible to get or set the matrix
## and it's inverse. The assumption is that the input matrix is invertible

## Create a list containing the following functions using x as the matrix
##   (1) set the value for the matrix
##   (2) get the value of the matrix
##   (3) set the value of the inverse
##   (4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse of a matrix created with makeCacheMatrix, checking
## first to see if the inverse has already been cached. If so, get the inverse
## from the cache and skip computation, otherwise, calculate the inverse of the
## matrix and store it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
