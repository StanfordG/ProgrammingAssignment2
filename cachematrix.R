## Program to compute the inverse of a matrix and cache the result
## Subsequent calls to the function will return the cached result

## Function to store the matrix data (set) and the inverse (setinv) 
## and return these using get for the data or getinv for the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function tries to retrieve a cached inverse of the matrix (getinv)
## If the cache doesn't exist, it will calculate the inverse and 
## store the result (setinv). If called again on the same matrix, 
## this function will return a cached copy of the inverse instead 
## of calculating it again
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
