## Matrix object (makeCacheMatrix) and corresponding caching function (cacheSolve) 
## for matrix inversion to avoid repeating expensive computation.

## makeCacheMatrix: Creates matrix object that allows caching of inverse.
##      --set: Set the matrix.
##      --get: Get the matrix.
##      --setinverse: Set the cached inverse.
##      --getinverse: Get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        # For new object, set initial inverse to NULL.
        inv <- NULL
        
        # Method to set the current matrix.
        # Also sets inverse to NULL since the matrix has changed, therefore
        #       the cached inverse is no longer correct.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Method to get the current matrix.
        get <- function() x
        
        # Method to set the cached inverse.
        setinverse <- function(inverse) inv <<-- inverse
        
        # Method to get the cached inverse.
        getinverse <- function() inv
        
        # Returns the list of methods.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve: Adds cache retrieval to built-in "solve" function for matrix inverse.
##              Only applicable to makeCacheMatrix objects.

##      --Calculates the matrix inverse (built-in "solve" function) but first
##              checks if it is stored in cache to speed up calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the inverse from the x object. 
        inv <- x$getinverse()
        
        # If it is a new object or new data then the inverse will be NULL.
        # Otherwise return the cached inverse.
        if(!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }
        
        # If NULL, then we need to calculate the inverse.
        # x is a makeCacheMatrix object, so use the get() method.
        data <- x$get()
        
        # Built-in solve command to calculate matrix inverse.
        inv <- solve(data, ...)
        
        # set() method to store the matrix inverse in cache.
        x$setinverse(inv)
        
        # Return the inverse.
        inv
}

