## Functions to compute or retrieve the inverse of a
## matrix and caching the computed value

## makeCacheMatrix creates a list of functions as follows:
## set   :  caches the value of the matrix  
## get   :  retrieves the value of the matrix 
## setinv:  caches the value of the inverse matrix
## getinv:  retrieves the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set,       get = get,
             setinv = setinv, getinv = getinv)
}


## cacheSolve inverts the cached matrix, checking first if 
## the inverse has already been computed. If so, it retrieves
## the inverse from the cache instead of computing it again

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
