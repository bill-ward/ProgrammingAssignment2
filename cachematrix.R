## This file is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  q <-NULL
  set <-function(y){
    x <<-y
    q <<-NULL
  }
  
  get <-function() x
  
  setMatrix <-function(solve) q <<- solve
  getMatrix <-function() q
  
  list(set=set, get=get,
       setmMatrix=setMatrix,
       getMatrix=getMatrix)
  
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  q <-x$getMatrix()
  if(!is.null(q)){
    message("retrieving cached data")
    return(q)
  }
  
  data <-x$get()
  q <-solve(data, ...)
  x$setMatrix(q)
  q
}
