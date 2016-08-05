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
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Write a short comment describing this function
## cacheSolve computes the inverse of a given matrix, but checks first if the inverse was already calculated.
## if it was already calculated, it retreives it from cache, and avoids doing the calculation.  
## if not cached, it calculates it and caches it. 

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
