## Functions to:
##     1. create and access a matrix and its inverse.
##     2. calculate and store the inverse of a matrix.



##  This function creates a list of functions to store and get both a Matrix and its Inverse


makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This Functions take the matrix stored using the previous function and calculates and stores its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
  
  
}




