#The objective of this Programming assignment is to cache a matrix and its inverse
#   and return the cached inverse if already computed, 
#   else return its freshly computed inverse

#There are two functions in this assignment: makeCacheMatrix and cacheSolve

#The function makeCacheMatrix obtains an input as matrix and caches 
#   it and its inverse
makeCacheMatrix <- function(xmat = matrix()) {
    
    #initializing Cache value to NULL
    mcache <- NULL           
    
    #set function to cache the input matrix
    set <- function(ymat) {
        xmat <<- ymat
        mcache <<- NULL
    }
    
    #get function to obtain the stored cache
    get <- function() xmat
    
    #setinv function to cache the solve function
    setinv <- function(solve) mcache <<- solve
    
    #getinv function to obtain the cached inverse
    getinv <- function() mcache
    
    #Returning the Output list with functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#The function cacheSolve obtains an input as list got from the function makeCacheMatrix
#   and returns the inverse of the matrix

#If inverse is cached, it returns the cached inverse, else computes the inverse 

cacheSolve <- function(xmat, ...) {
    
    #using the getinv() function to get the cached inverse
    mcache <- xmat$getinv()
    
    #if the same is not null, using the cached inverse
    if(!is.null(mcache)) {
        message("getting cached data")
        return(mcache)
    }
    
    #if the same is null, computing the inverse
    matdata <- xmat$get()
    mcache <- solve(matdata, ...)
    
    #caching the inverse for future purposes
    xmat$setinv(mcache)
    
    #returning the computed inverse
    mcache
}