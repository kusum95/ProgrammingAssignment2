## Put comments here that give an overall description of what your
## functions do -- This R code is basically used to cache the inverse 
## of a matrix so that it can be used later when required, without 
## having to recalculate it everytime 

## Write a short comment describing this function -- The function 
## makeCacheMatrix is used to create a special matrix object,
## that can be used to cache the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function -- The function 
## cacheSolve is used to calculate the inverse of the special matrix 
## object returned by the makeCacheMatrix function above. If the 
## inverse is already calculated, then it returns the inverse value 
## stored in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


