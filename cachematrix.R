## This file has 2 functions namely makeCacheMatrix which makes a custom matrix having a set of functions ## associated with it to cache the output of the inverse of that matrix.

## Make Custom Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setSolve <- function(inverse) inv <<- inverse
  getSolve <- function() inv
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Cache the matrix and if already cached then just fetch the previous result

cacheSolve <- function(A, ...) {
  inv <- A$getSolve()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- A$get()
  inv <- solve(data, ...)
  A$setSolve(inv)
  inv
}
