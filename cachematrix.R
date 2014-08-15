## These two functions calculate the inverse of a square matrix
## and cache the result so it does not need to be calculated again.

## makeCacheMatrix constructs an object with a matrix and its inverse (null at object creation)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inver) inv <<- inver
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve caches the result of the solve function (matrix inversion). If the function is called again
## after calculating the inverse, it returns the cached data.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
