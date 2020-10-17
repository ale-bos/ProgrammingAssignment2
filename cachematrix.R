## The following functions allow you to create a matrix 
## with the functionality of calculate and caching the 
## inverse of it and avoiding recalculation. 

## This function allows to create a matrix with the 
## ability of storing in cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function returns the inverse of a matrix 
## created with the above function. If the inverse
## was not previously calculated, the function calculates,
## stores in cache and returns it. Otherwise, it returns the
## previously calculated inverse stored in cache

cacheSolve <- function(x, ... ) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  y <- x$get()
  i <- solve(y, ...)
  x$setInverse(i)
  i
}
