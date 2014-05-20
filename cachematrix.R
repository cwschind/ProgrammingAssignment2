## These two functions will take the inverse of a inputed matrix and store the result
## in cache for later use

## This function make the cache for the matrix 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                                   ##Set initial matrix to NULL, s will contain the inverse matrix
  set <- function(y) {
    x <<- y                                   ##Make x available in other environments
    s <<- NULL                                ##Make x available in other environments
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve   ##Set getinverse to solve matrix
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function take the matrix input and 
## tests whether it has been solved before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {                   ##Determnine if prviously solved
    message("getting cached data")
    return(s)                         ##If so return solved matrix
  }
  data <- x$get()
  s <- solve(data, ...)               ##If not calculate inverse with solve function
  x$setinverse(s)
  s
}
