## Provide a wrapper object for a given invertible matrix to cache the inverse once it is computed.
## The wrapper "object" consists of a list of named functions as follows:
##  "set" sets the matrix in the wrapper
##  "get" returns the original matrix
##  "setinverse" sets the cached inversion of the matrix 
##  "getinverse" returns the stored inversion of the matrix
## Example of intended usage:
##
##    m2 <- matrix(rnorm(1:1000000), 1000, 1000)
##    ex2 <- makeCacheMatrix(m2)
##    i2 <- cacheSolve(ex2)
##    identical(solve(m2),ex2$getinverse())
##    [1] TRUE
##


## function makeCacheMatrix assembles and returns the wrapper list for a given matrix that provides for 
## caching of the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The cacheSolve function returns the cached inverse if it exists. If it does not exist, the inverse
## is computed, cached, and then returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
