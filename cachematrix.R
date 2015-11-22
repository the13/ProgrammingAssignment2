## These functions cache the inverse of matrix
## Basically the functions act like `makevector` and `cachemean` described in [README.md](https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverseOfMatrix)
        inv <<- inverseOfMatrix
    getinverse <- function()
        inv
    list(
        set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
