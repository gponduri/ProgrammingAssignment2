## Put comments here that give an overall description of what your
## functions do

## The following function creates a special "matrix", 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored inverse value to NULL
    inverse <- NULL
    
    # to set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL   # since the matrix changed
    }
    # to get the value of the matrix
    get <- function() x
    # to set the inverse
    setinv <- function(inverse_) inverse <<- inverse_
    # to get the inverse, returns the inverse of matrix
    getinv <- function() inverse
    
    # return a list of all the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}


## The following function calculates the inverse of the special 
## "matrix" created with the above function. The function first 
## checks if the inverse has already been calculated. 
## If yes, it gets the inverse of matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
    # check if the inverse is already cached
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # not cached, so we get the matrix into data
    data <- x$get()
    # and compute the inverse
    inverse <- solve(data, ...)
    # then cache the inverse
    x$setinv(inverse)
    # and return it as well
    inverse
}
