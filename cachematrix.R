## These functions are used to cache the inverse of a matrix.
## This is useful for expensive computations where we want to avoid
## recalculating the inverse of the same matrix multiple times.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse cache when matrix is changed
    }
    
    # Function to get the value of the matrix
    get <- function() x
    
    # Function to set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Function to get the value of the inverse
    getinverse <- function() inv
    
    # Return a list of the four functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it.
## Otherwise, it computes the inverse and stores it in the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Try to get the cached inverse
    
    # If the inverse is already cached, return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # Otherwise, compute the inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)  # Compute the inverse using solve()
    x$setinverse(inv)        # Cache the result
    inv                      # Return the computed inverse
}
