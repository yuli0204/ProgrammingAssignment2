## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL;
  setM <- function(y) {
    x <<- y
    m <<- NULL
  }
  getM <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list (setM = setM, getM = getM, 
        setsolve = setsolve,
        getsolve = getsolve)

}


## Write a short comment describing this function
## The following function calculates the inverse of the 
## special "matrix" created with the above function.
## However, it first checks to see if the inversioin has 
## already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and
## sets the value of the inverse matrix in the cache via
## the setsolve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- makeCacheMatrix(x)$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  data <- makeCacheMatrix(x)$getM()
  m <- solve(data,...)
  makeCacheMatrix(x)$setsolve(m)
  m
}
