## This file contains the code for the function makeCacheMatrix and cacheSolve that
## allows to cache the inverse of a matrix

## This function provides a data structure which embeds the functions for read/write access
## to a matrix and to its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## By default inv is NULL
    inv <- NULL

    ## Setting the data
    set <- function(y) {
        x <<- y

        ## If the data is changed, the current value of inv is out of date
        ## It has to be recomputed
        inv <<- NULL
    }

    ## Accessing the data
    get <- function() x

    ## Setting inv
    setinverse <- function(inverse) inv <<- inverse

    ## Getting inv
    getinverse <- function() inv

    ## Making all this available
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is an implementation which optimizes the computation of the inverse of
## a matrix. The computation is only performed if no cached matrix inverse is available. Otherwise,
## the caches matrix inverse is directly returned.
## The function assumes the original matrix is invertible

cacheSolve <- function(x, ...) {
    ## Checking if a cached value is available
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## A cached value is available.
        ## It is returned without any computation
        message("getting cached data")
        return(inv)
    }
    ## No cached value is available
    data <- x$get()

    ## Let us compute the inverse
    inv <- solve(data, ...)

    ## Let us cache the inverse
    x$setinverse(inv)
    
    ## return the value
    inv
}

