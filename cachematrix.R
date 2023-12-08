
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinver = setinver, getinver = getinver)
}

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinver()
  
  if (!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  
  x$setinver(inv)
  inv
}


