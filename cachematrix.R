## Put comments here that give an overall description of what your
## functions do
## These functions allow you to cache the inverse of a matrix to avoid 
## recalculating it multiple times, which can be computationally expensive.

## makeCacheMatrix: Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL

  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse cache when the matrix is changed
  }

  # Function to get the value of the matrix
  get <- function() x

  # Function to set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse

  # Function to get the value of the inverse
  getinverse <- function() inv

  # Return a list of the above functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # Otherwise compute the inverse, cache it, and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
