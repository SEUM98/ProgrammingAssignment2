## Caching the Inverse of a Matrix
## This script defines two functions: `makeCacheMatrix` and `cacheSolve`.
## These functions help cache the inverse of a matrix to optimize performance.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse when new matrix is set
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse
    getinverse <- function() inv
    
    # Return a list containing the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Get cached inverse
    
    # If the inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If not cached, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)  # Compute inverse
    x$setinverse(inv)  # Cache the inverse
    inv  # Return the inverse
}
