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
  
  list(get=get,set<-set,getInversion=getInversion,setInversion=setInversion)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mc<-makeCacheMatrix(x)
  v <- mc$getInversion()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- mc$get()
  v <- solve(data, ...)
  mc$setInversion(v)
  v
}
