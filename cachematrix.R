## Functions to cache the inverse of a matrix, thus avoiding costly recomputation.

## Creates a special "cache matrix", which is really a list of functions to get/set 
## a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL

  set <- function(y) {
    if (!identical(x, y)) {
      x <<- y
      inv <<- NULL
    }
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Calculates the inverse of the special "cache matrix" created with the above function. 
## If the mean has already been calculated, it gets the inverse from the cache and skips the computation. 

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setinverse(inv)
  inv
  
}
