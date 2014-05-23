## the make cache matrix function creates a set of functions 
## that can later be called

## makeCacheMatrix initializes four functions

makeCacheMatrix <- function(x = matrix()) {
       # set local variable m (the cache) to be NULL
        m <- NULL
        
        # y is assigned to x in the parent environment 
        # (which is the makeCacheMatrix function)
        # the cache (m) is cleared in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # the get function returns the variable x
        get <- function() x
        
        # the cache (m) is assigned the value of the inverse 
        # in the parent environment
        setinverse <- function(inverse) m <<- inverse
        
        # getinverse returns the variable m (in the local environment)
        getinverse <- function() m
        
        # a list is defined with the name of the four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the cacheSolve checks to see if the cache is empty, if it is, it will 
## calculate the inverse of the given matrix. If the cache is not empty,
## it will return the value in the cache without finding the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        # check if m is null, or has a value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # m is null, so find the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
