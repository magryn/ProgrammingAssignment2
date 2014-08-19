## The two functions below first create a number of functions and cached values,
## and then operate on these functions and cached values to either retrive a cached
## value for the inverse of a matrix, or calculate the inverse and cache it. 

## creates 4 operator functions, caches a matrix, and passes a list with these
## values as a special cached matrix object to be called by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getmatrix <- function() x
    setinv <- function(z) inv <<- z
    getinv <- function() inv
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinv = setinv,
         getinv = getinv )
    
}


## This function takes a list created by makeCacheMatrix and checks the inverse value
## stored. If the inverse value is not NA, cacheSolve will return it. Otherwise, it will
## calculate this value using the solve() function to operate on the matrix provided by
## getmatrix(). Note: if the stored matrix is changed with setmatrix(), then the stored inverse
## is reset to NULL and so it is necessary to recalculate this value each time a matrix changes.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ourMatrix <- x$getmatrix()
    inv <- solve(ourMatrix)
    x$setinv(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}
