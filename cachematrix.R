## These functions are designed to cache and report the inverse of a matrix


## the following function sets up an environment that can be used to find the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        temp <- NULL
        set <- function(y) {
                x <<- y
                temp <<- NULL
        }
        get <- function() x
        setinv <- function(solve) temp <<- solve
        getinv <- function() temp
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function can retrive the information cached in the environment created by the previous function to return the inverse of the matrix

cacheSolve <- function(x, ...) {
        temp <- x$getinv()
        if(!is.null(temp)) {
                message("getting cached data")
                return(temp)
        }
        data <- x$get()
        temp <- solve(data, ...)
        x$setinv(temp)
        temp
}
