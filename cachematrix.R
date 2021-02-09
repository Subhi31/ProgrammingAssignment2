## In this particule case, the inverse of the matrix is cached, so that the function can be used repeatedly with different inputs.




## makeCacheMatrix creates an object to cache of the matrix by performing the following functions.

## set() helps to set the value of the matrix.
## get() helps to get the value of the matrix.
## setinerse() helps to set the value of the inverse of the matrix.
## getinverse() helps to get the value of the inverse of the matrix.




makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.




cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
