## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## The <<- operator is used to assign a value to an object in an 
## environment that is different from the current environment. 

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing functions to:
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse of the matrix
##  - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the cache of the inverse to NULL
    inverse <- NULL
    ## 'set' function: to set a new value 'y' to the matrix stored in
    ## the specal "matrix". Note that when a new value is provided, we
    ## re-initialize the cached value of the inverse to NULL value.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## 'get' function: to get the current value of the matrix stored in
    ## the special "matrix"
    get <- function() {
        x
    }
    ## 'setinverse' function: to cache the inverse of the matrix
    setinverse <- function(i) {
        inverse <<- i
    }
    ## 'getinverse' function: to get the cache of the inverse of the matrix
    ## if setinverse has never been called before, NULL will be returned
    getinverse <- function() {
        inverse
    }
    ## return the list of all the subfunctions available in the special "matrix"
    list(
        set = set, 
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## cacheSolve returns a matrix that is the inverse of 'x'

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve retrieve the inverse from the cache.

## For this assignment, we assume that the matrix supplied
## is always invertible.

cacheSolve <- function(x, ...) {
    ## First we get the value of the cached inverse of the special
    ## "matrix" 'x', using the subfunction 'getinverse'
    inverse <- x$getinverse()
    ## If the inverse of the matrix has never been cached before, 
    ## we get a NULL value.
    if(!is.null(inverse)) {
        ## If the cached value is not NULL, this means the cached 
        ## value has already been calculated. So we print the
        ## following message to the user. Note that in both case, 
        ## the value is returned at the end of the function.
        message("Inversed matrix already computed... Getting cached data...")
    } else {
        ## If the retrieved cached value is NULL, then we calculate
        ## it using the R function solve(). Then we put the result 
        ## in the special "matrix" 'x' (using 'setinverse' subfunction)
        ## in order it could be reused in the future 
        message("No cache found for the inversed matrix... Inversing matrix...")
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
    }
    ## At the end, the retrieved (or calculated) value of the inverse
    ## of the special matrix is returned.
    inverse
}

