## Caching the Inverse of a Matrix
##  
## The assignment is to write a pair of functions that cache the inverse of a matrix.


## This routine creates a wrapper structure for a matrix that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## the cached inverse, or null if not present
  inv <- NULL
  
  # sets the matrix to be wrapped and invalidates the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # returns the wrapped matrix
  get <- function() x
  
  # store the inverse matrix in the cache
  setinverse <- function(inverse) inv <<- inverse
  
  # returns the cqched inverse matrix (may return null)
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This routine either calculates the inverse of the matrix wrapped by 
## the argument x  and caches the inverse in the wrapper structure,
## or returns the cached result.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  inv
}
