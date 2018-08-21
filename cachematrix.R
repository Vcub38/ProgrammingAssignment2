## The following two functions, makeCachematrix() and cacheSolve(), cache the inverse of a matrix.

## Define the set(), get(), setinverse(), and getinverse() functions and
##  return a list of these four functions to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## note: function argument, y, is assumed to be a numeric, square, invertible matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Attempt to retrieve the inverse of the input argument. If a cached matrix is retrieved return it
## to the parent environment, otherwise calculate the inverse of the matrix, set the inverse in cache
## and return the calculated inverse to the parent environment. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}