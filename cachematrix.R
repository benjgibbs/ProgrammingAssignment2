## These functions optimise matrix inversion by memoisation

## Creates a special environment for storing a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    list(
        set = function(y){
            x <<- y
            i <<- NULL
        },
        get = function() x,
        setinverse = function(inverse)  i <<- inverse,
        getinverse = function() i
    )
}


## Either computes the inverse and returns it or if previously calculated
## returns the cached value

cacheSolve <- function(x, ...) {
    debug
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
