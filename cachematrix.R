## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL  # Initialize the cache for the inverse as NULL
  
          set <- function(y) {
            x <<- y
            inv <<- NULL  # Clear the inverse cache if the matrix changes
          }
          
          get <- function() x
          
          setInverse <- function(inverse) inv <<- inverse
          
          getInverse <- function() inv
          
          list(set = set, get = get, 
               setInverse = setInverse, 
               getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
          if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
          }
          # Compute the inverse as it's not available in cache
          data <- x$get()
          inv <- solve(data, ...)  # This computes the inverse
          x$setInverse(inv)  # Cache the inverse for future use
          inv
}
