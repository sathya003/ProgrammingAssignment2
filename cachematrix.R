#The objective of this Programming assignment is to 
#   1. cache a matrix and its inverse
#   2. return the cached inverse if already computed, 
#   3. else return its freshly computed inverse

#There are two functions in this assignment: 
#   1. makeCacheMatrix 
#   2. cacheSolve

#Use the output of the function makeCacheMatrix as an input to cacheSolve

#FUNCTION: makeCacheMatrix 
#INPUT: matrix
#OUTOUT: list
#OBJECTIVE: obtains an input as matrix and returns list of functions

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


#FUNCTION: cacheSolve 
#INPUT: list
#OUTOUT: matrix
#OBJECTIVE: obtains an input as list of functions and 
#   returns the inverse of matrix given as input to makeCacheMatrix

cacheSolve <- function(retmatlist, ...) {
    
    #using the getinv() function to get the cached inverse
    mcache <- retmatlist$getinv()
    
    #if the same is not null, using the cached inverse
    if(!is.null(mcache)) {
        message("getting cached data")
        return(mcache)
    }
    
    #if the same is null, computing the inverse
    matdata <- retmatlist$get()
    mcache <- solve(matdata, ...)
    
    #caching the inverse for future purposes
    retmatlist$setinv(mcache)
    
    #returning the computed inverse
    mcache
}