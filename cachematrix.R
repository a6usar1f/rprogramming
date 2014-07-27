## cachematrix.R : A script to compute and cache the inverse of a matrix rather
##                 than computing it repeatedly. To achieve this purpose, a pair
##                 of functions is built to make the cached matrix and to com-
##                 pute its inverse. The matrix supplied is assumed to be always
##                 invertible.
## Agus Arif, 27-07-2014

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function solves the inverse of the special "matrix" returned by
## `makeCacheMatrix` above
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Getting cached inverse value")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
## Return a matrix that is the inverse of 'x'
  i
}
