## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function
## 
## Functions were adapted from makeCacheVector example.
##
## NAME: makeCacheMatrix
## DESC: creates a special matrix object that can cache its inverse.
##       matrix used must be a square invertable matrix.
## INPUTS:
##        Parameters:
##            square invertable  matrix
##
## OUPUT :
##         object that contains list of cached matrix functions included cached
##         inverse for the matrix.
## matrix object contains four internal functions.
##
## 
makeCacheMatrix <- function(x = matrix()) {
  ## m stores matrix inverse
  m <- NULL
  
  ##       set: takes new matrix as an input and clears m (inverse) 
  set <- function(y) {
    print("makeCacheMatrix set function called\n")
    x <<- y
    m <<- NULL
  }
  
  ##  get: returns matrix data
  get <- function() { 
    print("makeCacheMatrix get function called\n")
    x
  }
  
  ## setinverse: sets cached inverse (m) of matrix.
  setinverse <- function(thedata) {
    print("makeCacheMatrix setinverse function called\n")
    m <<- thedata
  }
  
  ## getinverse: returns the cached inverse matrix (m)
  getinverse <- function() {
    print("makeCacheMatrix getsolve function called\n")
    m
  }
  
  ## list of the functions to return
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}






## Write a short comment describing this function
## NAME: cacheSolve
## DESC: solves the inverse of matrix object created with makeCacheMatrix. Uses 
##       the cached inverse if available. If no cached inverse is available it solves the 
##       matrix and stores the value in the cache.
## INPUTS:
##        Parameters:
##            makeCacheMatrix object.
##
## OUPUT :
##         inverse for the matrix.
## cacheSolve uses functions from within the makeCacheMatrix object.
## EXAMPLE USAGE:
## yy <- matrix(rnorm(1000),10,10)
## xx <- makeCacheMatrix(yy)
## zz <- cacheSolve(xx)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    printf("using cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
