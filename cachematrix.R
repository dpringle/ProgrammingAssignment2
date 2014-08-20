## cachematrix.R 
## DP 20-Aug-2014
## Coursera Rprog Assignment 2

## Two functions defined here for finding the inverse of a matrix. 
## The result (inverse) is cached, so that if the inverse of the same matrix is requested again,
## then the value is returned from cache, not recalculated.


## makeCacheMatrix: This function creates a special "Vector" object with an input matrix, 
## and defines functions to set and get the matrix inverse.
## The inverse is NULL if not yet solved - which is used in cacheSolve function below.

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function is used to calculate the inverse of the underlying matrix input to makeCacheMatrix.

## Howevere its input argument is _not_ the underlying numeric matrix, but rather the "Vector" returned by the maekCacheMatrix above. 
## That "Vector" includes a cache of the inverse if that inverse has already been cached.  
## If that inverse "m" is NULL then the inverse is calculated and then cached for future use.


cacheSolve <- function(x, ...) {
  
        ## Retreive the inverse - and if it exists (is not NULL) 
        ## then return this cached value
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Otherwise, get the underlying matrix, find its inverse using solve, and 
        ## set this to be the cached inverse and return this inverse matrix m
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}