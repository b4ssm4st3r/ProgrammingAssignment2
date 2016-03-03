##  Two function. The first creates a special "matrix" object that can cache its inverse.
## The second takes the inverse of the result of makeCacheMatrix 
## cachesolve should retrieve the inverse from the cache if the inverse is already there.

## this one is supposed to set up the fancy matrix and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  matinv = NULL
  set = function(y) {
    x <<- y
    matinv <<- NULL
  }
  get = function() x
  setinv = function(inverse) matinv <<- inverse 
  getinv = function() matinv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this one is supposed to ge the resulting data from the matrix created and retrieve an already existing inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getInv() 
  if(!is.null(minv)) { 
    message("getting cached data")
    return(minv) 
  }
  data <- x$get() 
  minv <- solve(data) 
  x$setInv(minv) 
  return(minv) 
}
