## This file contains two functions: makeCacheMatrix and cacheSolve. Together they create
## a "matrix" file type that also has the ability to cache the value of its inverse. This can 
## save on computation time because it will retrieve the cached inverse value rather recalculating
## the value

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## (1) set value of the matrix (2) get value of the matrix (3) set value of the inverse and
## (4) get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),then cacheSolve  
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
