## Caching the Inverse of a Matrix
##

## Create special "matrix" which stores the matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(m) {
    x <<- m
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of the matrix using cached value when available

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
