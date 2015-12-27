## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setmatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getmatrix <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting the cached data")
                return(inverse)
                
        }
        mat <- x$getmatrix()
        inverse <- solve(mat, ...)
        x$setinv(inverse)
        inverse
}
