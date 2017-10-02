## This R Programming Assignment experiment is to write a pair of functions, namely, "makeCacheMatrix" and "cacheSolve" 
## that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  }

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

 cacheSolve <- function(x, ...) {		  
 ## Return a matrix that is the inverse of 'x'		         
 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }
## ==========================RUNNING THE SCRIPT FOR THE RESULT======================

x <- matrix(rnorm(9),3,3)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
            [,1]       [,2]       [,3]
[1,]  0.22937057  0.1548431  0.4804860
[2,] -0.42766031  0.1932592  0.4141140
[3,] -0.03426038 -0.6422278 -0.0893858
