## A pair of functions that will cache the inverse of a initial matrix.
## This function creates a special "matrix" object that can cache its inverse.
##============================================================================
## Original creator: RDPeng (date unk)
## Edited: Mike Segura (5/4/2018)
##============================================================================
## This initial function is a matrix which will be used to create a cache inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This secondary function will create the inverse function of the makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv 
}
