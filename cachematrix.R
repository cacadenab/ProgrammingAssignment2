## Functions to:
##     1. create and access a matrix and its inverse.
##     2. calculate and store the inverse of a matrix.



##  This function creates a list of functions to store and get both a Matrix and its Inverse


makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    InverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) InverseMatrix <<- Inverse
  getInverse <- function() InverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This Functions take the matrix stored using the previous function and calculates and stores its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InverseMatrix <- x$getInverse()
  if(!is.null(InverseMatrix)) {
    message("getting cached data")
    return(InverseMatrix)
  }
  data <- x$get()
  InverseMatrix <- solve(data, ...)
  x$setInverse(InverseMatrix)
  InverseMatrix
  
  
}




