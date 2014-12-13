## These functions provide an implementation of matrices that can
## cache their inverse, so that subsequent calls to cacheSolve()
## do not need to recompute the inverse

## makeCacheMatrix creates a new cached Matrix object. It provides
## functions get() and set() to access the underlying matrix object,
## and getInverse() and setInverse() to access the cached inverse.
## caution: getInverse() might return NULL. Use cacheSolve instead.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL # invalidate cache
  }
  getInverse <- function() inv
  setInverse <- function(inverse) inv <<- inverse
  
  list(get = get, set = set,
       getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve calculates the inverse of a cached matrix, using
## the cached version when possible. Same arguments as solve(), but
## takes a cacheMatrix as 'x'.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'    
  inv <- x$getInverse()
  if (!is.null(inv)) {
    print("cached")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}
