## This file contains 2 functions 
## 1) makeCacheMatrix: creates a list of functions to process a matrix 'x'
## 2) cacheSolve: Returns a matrix that is the inverse of 'x'

## 1) makeCacheMatrix receives matrix 'x' as parameter
##    and returns a list with following functions:
##    .set: sets the value of the matrix x
##    .get: gets the value of the matrix x
##    .setsolve: sets the value of the solve(x)
##    .getsolve: gest the value of the solve(x)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## 2) cacheSolve receives as parameter a list created by makeCacheMatrix
##    and returns the inverse of a matrix 'x' set in makeCacheMatrix    
##    first checks if the inverse of 'x' has been already calculate
##        if yes, returns the value from the cache using "getsolve" and 
##                avoids unnecessary calculation
##        if no,  calculates the inverse and 
##                sets the result in the cache using "setsolve"   

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  
  ## verifies if the result is already in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## otherwise, calculates the inverse and save the result in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## unit testing data set
## mm<-matrix(c(2, 4, 3, 1),nrow=2, ncol=2)
## mmm<-matrix(c(2, 4, 3, 1,5, 6, 0,8,9),nrow=3, ncol=3)

