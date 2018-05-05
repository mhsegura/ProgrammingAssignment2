## A pair of functions that will cache the inverse of a initial matrix.
## This function creates a special "matrix" object that can cache its inverse.
##===============================================================================
## Original creator: RDPeng (date unk)
## Edited: Mike Segura (5/4/2018)  
##===============================================================================
## This initial function is a matrix which will be used to create a cache inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This secondary function will create the inverse function of the makeCacheMatrix.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$getMatrix()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  invMatrix 
}
