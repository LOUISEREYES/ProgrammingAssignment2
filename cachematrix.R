## The inverse of a matrix is cached by a pair of functions

## Creates a special matrix object that can cache its inverse for the input (that is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        l <- NULL
        set <- function (matrix) {
                m <<- matrix
                l <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) {
                l <<- inverse
        getInverse <- function() l
        list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}
        
##This is used to get the cache data

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}
