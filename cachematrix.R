## cachematrix.R 
## DP 20-Aug-2014
## Coursera Rprog Assignment 2
##
## Two functions defined here for finding the inverse of a matrix. The result (inverse) is cached, so that if 
## the inverse of the same matrix is requested again, it can be returned from cache, not recalculated.
## 
## --
##
## makeCacheMatrix: this function creates a special "Vector" object with an input matrix, and defines functions 
## to set and get the matrix inverse. The inverse is NULL if not yet solved - which is used in cacheSolve function below.

makeCacheMatrix <- function(x = numeric()) {
        
        ## 1. Initialise inverse m as NULL
        
        m <- NULL
        
        ##  2. Define set function which assigns input y to x and assigns NULL value to inverse m
        ## and get function to return underlying matrix input to makeCacheMatrix
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        ## 3. Define setinverse and getinverse functions to solve and return matrix inverse m
        
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## 4. Ouput of function is a list of functions with names $set, $get, $setinverse, $getinverse defined within
        ## the makeCacheMatrix function environment
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: this function is used to calculate the inverse of the underlying matrix input to makeCacheMatrix.
## Input argument is _not_ the underlying numeric matrix, but rather the "Vector" returned by the makeCacheMatrix above. 
## That "Vector" includes a cache of the inverse if that inverse has already been cached, which can be returned directly.  
## If that inverse "m" is NULL then the inverse is calculated and cached for future use.


cacheSolve <- function(x, ...) {
        
        ## 1. Retrieve matrix inverse - and if it exists (is not NULL) then return this cached value
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Otherwise, get the underlying matrix, solve for and set teh inverse, and return this inverse matrix m
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}