## The makeCacheMatrix and cacheSolve functions, when used in conjunction, take a square invertible matix as input, calculate the inverse of the input matrix, and cache the resulting inverse for retrieval.

## This function takes a square invertible matrix as input and creates a new list object which stores the input matrix and creates placeholders for cacheing the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
    invmx <- NULL
    set <- function(y) {
        x <<- y
        invmx <<- NULL
    }
    get <- function() x
    setsolve <- function(solved) invmx <<- solved
    getsolve <- function() invmx
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## This function reads the list object created by the makeCacheMatrix function, calculates the inverse matrix (if one has not been calculated already), and then returns the inverse matrix that has been cached by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {

    invmx <- x$getsolve()
    if(!is.null(invmx)) {
        message("getting cached data")
        return(invmx)
    }
    mx <- x$get()
    invmx <- solve(mx)
    x$setsolve(invmx)
    invmx
}
