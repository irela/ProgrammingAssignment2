## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function takes matrix x and makes a cache matrix of it. Cache matrix
## has the matrix data and the inverse stored.  

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function takes cacheMatrix and returns it's inverse. First it 
## checks if the inverse has already been calculated and returns it. If not inverse is 
## solved and stored by calling cacheMatrix' setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
