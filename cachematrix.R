## The makeCacheMatrix function creates a special matrix object that can cache its inverse.
## It initializes the matrix with the given input 'x' (default is an empty matrix).
## This function returns a list of functions: set, get, setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  ## Initialize a variable to store the inverse of the matrix
    ## Define a function to set the matrix data
    set <- function(y) {
        x <<- y  ## Assign 'y' to 'x', using '<<-' to modify the variable in the parent environment
        inv <<- NULL  ## Reset the cached inverse since the matrix has changed
    }
    ## Define a function to retrieve the matrix data
    get <- function() x
    ## Define a function to set the cached inverse
    setinverse <- function(inverse) inv <<- inverse
    ## Define a function to retrieve the cached inverse
    getinverse <- function() inv
    ## Return a list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## this function retrieves the inverse from the cache instead of recomputing it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  ## Retrieve the cached inverse
    if (!is.null(inv)) {  ## If the inverse is cached
        message("getting cached data")  ## Display a message indicating cached data is being retrieved
        return(inv)  ## Return the cached inverse
    }
    data <- x$get()  ## Otherwise, retrieve the matrix data
    inv <- solve(data, ...)  ## Compute the inverse of the matrix
    x$setinverse(inv)  ## Cache the computed inverse
    inv  ## Return the computed inverse
}
