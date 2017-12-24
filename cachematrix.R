## Used makeCacheMatrix to create a matrix object. Then cacheSolve 
## calculates the inverse of that matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
      inv_x <- NULL
      set <- function(y) {
            x <<- y    # Set value
            inv_x <<- NULL # Clear cache
      }
      get <- function() x
      setInverse <- function(inverse) inv_x <<- inverse
      # function to get the inverse
      getInverse <- function() inv_x
      # Return a list with the above four functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      m <- x$getInverse() # Fetches the cached value for the inverse
      if(!is.null(m)) { # If the cache was not empty, we can just return it
            message("getting cached data")
            return(m)
      }
      # The cache was empty. We need to calculate it, cache it, and then return it.
      data <- x$get()  # Get value of matrix
      m <- solve(data) # Calculate inverse
      x$setInverse(m)  # Cache the result
      m                # Return the inverse
}
