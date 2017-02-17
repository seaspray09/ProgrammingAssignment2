# These functions using lexical scoping  to cache the inverse of a matrix.
# The function makeCacheMatrix creates a special "matrix, which is a list 
# containing a function to 1) set the value of the matrix, 2) get the value 
# of the matrix 3) set the value fo the inverse  matrix and 4) set the value 
# of the inverse matrix.  Then the cachesolve function is run to solve the 
# matrix from the cached data


# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    #initialize cache matrix to NULL
    inv <- NULL
    
    # define set method, to set the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # define get method, to get the matrix
    get <- function() x
    
    # define setinverse method, to set the inverse of the cache matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # define getinverse method, to get the inverser of the cache matrix
    getinverse <- function () inv
    
    # list names of all the methods to be visible
    list (set = set, get = get,
          setinverse = setinverse, 
          getinverse = getinverse)
}



# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    # checks content of the inverse cache matrix
    inv <- x$getinverse()
    
    # returns the inverse of cache matrix if content is not null
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      
    }
    
    # if content is empty, then get the matrix, create inverse matrix,
    # and set, update, and return the inverse of the cache matrix
    mx <- x$get()
    inv <- solve(mx, ...)
    x$setinverse(inv)
    inv  
       
}
