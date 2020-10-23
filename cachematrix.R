## Put comments here that give an overall description of what your
## functions do

## Functions:
## - makeCacheMatrix - A function to create a special "matrix" object and 
##                      cache its inverse
## - cacheSolve -  A function which return an inverse of the 'matrix' object
##                  if it already exist in the cache and the object remain 
##                  unchanged. Else, the function will calculate the inverse
##                  on the special 'matrix' returned by makeCacheMatrix.
## Write a short comment describing this function

#Short Comment
## A function that received a special "matrix" object and cache the inverse of the matrix and store in the global environment
## It only set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## Write a short comment describing this function
## This function will do these:
## > Check if the inverse object exist and the matrix is not changed by getting
##    the stored inverse matrix calling from the function makeCacheMatrix
## > Returns the inverse object in cache if it satisfy the first rules.
## > Else, compute the inverse of the special "matrix" and store in 
##    makeCacheMatrix for future use and return the new inverse of "x"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
