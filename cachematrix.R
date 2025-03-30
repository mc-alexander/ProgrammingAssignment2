## Put comments here that give an overall description of what your
## functions do

# This function creates a special 'matrix' object that caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse property
    inv <- NULL
    
    # Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Invalidate cache when matrix changes
    }
    
    # Method to get the matrix
    get <- function() x
    
    # Method to set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Method to get the inverse
    getInverse <- function() inv
    
    # Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix. If the inverse has already been calculated (and the matrix
# hasn't changed), it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Get the current inverse
    inv <- x$getInverse()
    
    # If inverse is already cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    # Return the inverse
    inv
}
