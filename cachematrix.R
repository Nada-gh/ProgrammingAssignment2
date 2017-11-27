## function makeCacheMatrix creates a square matrix. set the value of the matrix,and then get the matrix back.
## The use of "<<-" operator to assign a value to an object defined in another environment. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(inver) inv <<- inver
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## function cacheSolve checks first if the inverse of the matrix X is already cached. If so, returns it. Otherwise, the inverse
## is calculated using the "solve" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
