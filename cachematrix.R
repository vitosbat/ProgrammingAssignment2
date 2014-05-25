
## This function create list with 4 functions: set&get matrix, 
## and set&get inverse matrix. 
## In sets functions if value had not changing it takes from the cache, 
## otherwise assign a new value.  
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function return inverse matrix 'x'.
## It checks: if inverse matrix value was calculated before,
## it takes it from the cache, otherwise it calculates a new value.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
