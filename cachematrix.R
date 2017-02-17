# These functions using lexical scoping  to cache the inverse of a matrix.
# The function makeCacheMatrix creates a special "matrix, which is a list 
# containing a function to 1) set the value of the matrix, 2) get the value 
# of the matrix 3) set the value fo the inverse  matrix and 4) set the value 
# of the inverse matrix.  Then the cachesolve function is run to solve the 
# matrix from the cached data


# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function () inv
    list (set = set, get = get,
          setinverse = setinverse, 
          getinverse = getinverse)
}



# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      
    }
    
    mx <- x$get()
    inv <- solve(mx, ...)
    x$setinverse(inv)
    inv  
       
}
