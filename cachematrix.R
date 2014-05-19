##  Caching the Inverse of a Matrix
## 

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the solve
# get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- - y
                m <<- - NULL
        }
        get <- function() x
        setsolve <- function(solve) m = - solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve calculates the solve of the special "matrix"
# It first checks to see if the solve has already been calculated
# If so, it gets the solve from the cache and skips the computation
# Otherwise, it calculates the solve of the data and sets the value 
# of the solve in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
