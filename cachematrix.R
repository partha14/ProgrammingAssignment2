## Put comments here that give an overall description of what your
## functions do

## this function creates and returns four functions: set() to set the matrix, get() to get the matrix, setinv() to set the inverse
## and getinv() to get the inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function retrieves the inverse if already set, else computes the inverse from the object returned from the previous function

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        data %% m
        x$setinv(m)
        m
}
