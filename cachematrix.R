## Function makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,getInverse = getInverse)
}

## Function cacheSolve() calculates the inverse of the matrix passed by makeCacheMatrix()
## It returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
