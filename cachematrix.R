## Inverse Matrix Caching:
## Calculating inverse matrices can be time consuming and computationally intensive.
## Caching the inverse of a matrix, instead of continually computing it, is preferable.
## The following functions create an object that stores a matrix and caches its inverse.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

##The following creates a null object.
  invM <- NULL

## The following sets the value of the matrix.
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) invM <<- inverse
  getInv <- function() invM
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)

}


## This function computes the inverse of the matrix created by the previous function.
## If the inverse has already been computed, then the following function will give the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Returns the inverse matrix.

  invM <- x$getInv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  matdata <- x$get()
  invM <- solve(matdata, ...)
  x$setInv(invM)
  invM

}
