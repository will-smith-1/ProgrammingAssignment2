## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this is the makeCacheMatrix function. pretty damn cranky about it though.  Where was any of this in the lectures or Swirl?  The instructional design could use some work.  Creating gaps is good approach for triggering learing, but in this case, the gap is too large.

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
##this is the cacheSolve function

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
