## These functions provide support for caching the inverse of a matrix, 
##      in order to avoid potentially expensive (re)computation of the inverse
##
## Usage:
##     Given an invertible matrix "x"....
##
##     a. Create a "cache matrix" object - let's call it "cacheX" - 
##          corresponding to "x", by calling cacheX <- makeCacheMatrix(x).
##     b. Anywhere the inverse of "x" is needed, 
##          call myInverse <- cacheSolve(cacheX) to get it, 
##          instead of calling myInverse <- Solve(x).
##

##  makeCacheMatrix(x) - creates a "cache matrix" object for a given matrix "x".
##  This "cache matrix" object essentially stores the original matrix "x" 
##      and its inverse, providing accessor functions (get, set) for both
makeCacheMatrix <- function(x = matrix()) {
    ## Note: "x" and "inverse" are defined in the scope of the makeCacheMatrix 
    ##  function. Essentially, they define the state of a given instance of 
    ##  a "cache matrix" object, similar to fields/instance variables 
    ##  in OOP languages (Java, C#, for example)
    inverse <- NULL
    
    ## Setter functions
    ## The important note here is that both setters 
    ##  use the superassignment operator (<<-), 
    ##  as opposed to the regular assignment operator (<-)
    ##
    ## This way, they set values in the variables with those names 
    ##  in the enclosing scope of each setter function, 
    ##  which in this case are the above mentioned "instance variables" 
    ##  x and inverse
    ##
    ## If the regular assignment operator (<-) had been used, 
    ##  it would modify local (to each setter) objects x and inverse, 
    ##  and the setters would have no effect on the "cache matrix" object
    
    set <- function(y) {
        x <<- y
        ## inverse is invalidated every time x is set, 
        ##  on creation (call to makeCacheMatrix) or through the setter
        inverse <<- NULL
    }
    
    setinverse <- function(newinverse) inverse <<- newinverse
    
    ## Getter functions
    ## They will return "x" and "inverse" of the enclosing scope, 
    ##  since they are not defined in the functions themselves.
    get <- function() x
    getinverse <- function() inverse
    
    ## Return is a list of functions available in a given instance
    ##  of a "cache matrix" object
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

##  cacheSolve(x, ...) - returns the inverse of a given "cache matrix" object
##
##  Notes:
##    1. The underlying matrix is assumed to be invertible. 
##        If not, the function will produce an error
##    2. The extra parameters (...) are passed to the underlying solve function
##        Like solve, createSolve can also be used to calculate 
##        the solution of a linear system, if given a second matrix
cacheSolve <- function(x, ...) {
    ##  a. Check if there is a cached inverse, returning that if available
    ##  b. If not available, calculates the inverse and use the result 
    ##      to both update the cached value and return
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
