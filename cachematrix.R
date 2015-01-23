## This file is wirtten as a submission to Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  v<-NULL
  
  set<-function(y) {
    x<<-y
    v<-NULL
  }
  
  get<-function() x
  
  setInversion<-function(solve) v<<-solve
  
  getInversion<-function() v
  
  list(get=get,set=set,getInversion=getInversion,setInversion=setInversion)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## The argument 'x' here should be a function.  
  ## Return a matrix that is the inverse of 'x', here the 'x' refers to the argument of makeCacheMatrix().
  
  Inv <- x$getInversion()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInversion(Inv)
  Inv
}
