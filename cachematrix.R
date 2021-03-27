## Assignment 2

## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  set <- function(y) {
    x <<- y
    cachedinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedinverse <<- inverse
  getinverse <- function() cachedinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    cachedinverse <- x$getinverse()
    if(!is.null(cachedinverse)) {
      message("getting cached data")
      return(cachedinverse)
    }
    data <- x$get()
    cachedinverse <- solve(data, ...)
    x$setinverse(cachedinverse)
    cachedinverse
  }
