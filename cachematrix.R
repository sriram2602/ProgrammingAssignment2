## Caching the Inverse of a Matrix:
## The two functions below provide the ability to cache the inverse
## of a matrix. This avoids repeated computation effort.

## Create a special "matrix" object that can cache the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Computes the inverse of the special "matrix" created above. 
## If inverse has already been computed, it returns that value from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting inverse from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}    