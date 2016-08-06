## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix generates a list containing options to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## these options are used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve computes the inverse of a given matrix, but checks first if the inverse was already calculated.
## if it was already calculated, it retreives it from cache, and avoids doing the calculation.  

cacheSolve <- function(x, ...) {
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached inverse matrix")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
