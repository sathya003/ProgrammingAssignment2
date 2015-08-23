#The objective of this Programming assignment is to cache the Inverse of a matrix

#There are two functions in this assignment: makeCacheMatrix and cacheSolve

#The function makeCacheMatrix obtains an input as matrix and caches its inverse
makeCacheMatrix <- function(xmat = matrix()) {
    mcache <- NULL
    set <- function(ymat) {
        xmat <<- ymat
        mcache <<- NULL
    }
    get <- function() xmat
    setinv <- function(solve) mcache <<- solve
    getinv <- function() mcache
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#The function cacheSolve obtains an input as matrix and returns its inverse

#If it is cached, it returns the cached inverse, else computes the inverse 

cacheSolve <- function(xmat, ...) {
    ldet<-determinant(xmat)
    if(ldet$modulus==-Inf) {
        print("Inverse cannot be calculated")
    }
    mcache <- xmat$getinv()
    if(!is.null(mcache)) {
        message("getting cached data")
        return(mcache)
    }
    matdata <- xmat$get()
    mcache <- Solve(matdata, ...)
    x$setinv(mcache)
    mcache
}