## This file contains the two R functions to complete this assignment

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # create structure with the original matrix and the inverse matrix (NULL when not computed yet)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # method that returns the original matrix
  get <- function() x
  # inverse setter method
  setinverse <- function(inverse) m <<- inverse
  # inverse getter method
  getinverse <- function() m
  # return list with necessary data
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  # x is a "matrix" returned via 'makeCacheMatrix'
  # Use getinverse method on x
  m <- x$getinverse()
  # if the original matrix has already been computed into cached inverse matrix, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise, compute it (get original matrix via 'get' and use solve) and store it for future use via setinverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  # return the inverse matrix
  m
}
