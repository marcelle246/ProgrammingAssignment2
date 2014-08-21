## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.This is done by:
## a) setting the value of the matrix;
## b) getting the value of the matrix;
## c) setting the value of the inverse matrix; and
## d) getting the value of the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x<<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<-inverse
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data,...)          ## calculate the inverse of the matrix
  x$setInv(i)
  i                             ## return the inverse of the matrix
}
