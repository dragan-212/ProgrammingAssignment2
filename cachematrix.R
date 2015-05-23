##
## cachematrix.R contains two functions: 
##      makeCacheMatrix: stores & retrieves the value of a matrix using "<<-" assignment
##          operator, essentially creating a cache by exposing values outside of
##          makeCacheMatrix's environment for other functions, like cacheSolve, to access;
##      cacheSolve: computes the inverse of a matrix. 
## 
## How to use: 
## create a square matrix using this example:
##    > m <- matrix(c(1, 2, 1, 3), 2, 2)
## store the matrix in cache via makeCacheMatrix as follows
##    > x <- makeCacheMatrix(m)
## to see the contents of the matrix in cache:
##    > x$get()
## to compute the inverse:
##    > cacheSolve(x)
## try agan to see if results come from cache:
##    > cacheSolve(x)
##
## NOTE: Assumes that matrix supplied is invertible. No checks are made to validate.
##

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix takes a matrix as imput and essentially stores its contents in cache.
## There are four functions in makeCacheMatrix that perform the following functions:
##      set: stores the value of the matrix in cache, exposing its contents outside the 
##          current environment;
##      get: returns the contents of the matrix from cache;
##      setinverse: stores the inverse value of the matrix, exposing its contents outside
##          the current environment;
##      getinverse: returns the inverse value of the matrix from cache, if calculated.
##
    
##  Initialize inverse matrix value (i) to NULL when matrix is initially stored in cache.
    i <- NULL
    
## Invoking "set" function requires a new matrix to be entered as input (y) and uses the
## "<<-" assignment operator to expose the value of x outside this function.
## When "set" is used to reset the matrix, we also need to reset the cached inverse value (i).
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

## invoking "get" function will return the contents of the matrix (x) from cache.
    get <- function() {
        x
    }

## Invoking "setinverse" will store the claculated inverse matrix in cache (i).
## By using the "<<-" assignment operator, we expose the value of i ouside this function
## essentially creating a cache.
    setinverse <- function(inv) {
        i <<- inv
    }

## invoking "getinverse" will return the inverse matrix (i) from cache.
## If it hasnt been calculted the return value will be NULL.
    getinverse <- function() {
        i
    }

## define the list of functions available in makeCacheMatrix.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
## cacheSolve takes a matrix as input, and attempts to compute its inverse. It first checks 
## whether the result has already been calculated and stored in cache.
##     If it hasnt, the inverse is calculated, stored in cache and returned. 
##     If it exists, cacheSolve returns the stored value from the cache.
##
## Note that we are storing the matrix (x) and its inverse (i) in makeCacheMatrix 
## uaing the "<<-" assignment operator, exposing their values to function cacheSolve.
##
 
## use makeCacheMatrix's "getinverse" function to retrieve the current inverse matrix value.
    i <- x$getinverse()

## if the inverse hasnt been calculated, use makeCacheMatrix's "get" function to retrieve
## the matrix, calculate its inverse using the solve() function and store the result in cache
## via makeCacheMatrix's "setinverse" function.
    if(is.null(i)) {
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
    }

## if the inverse has been calculated, inform the user inverse is being retrieved from cache.
    else {
        message("getting cached data")
    }

## Now return the inverse matrix. 
    return(i)
}
