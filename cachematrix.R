#Solution for the programming assignment 2

# This function creates a list with a set of functions to speed calculations avoiding repetition.
# get = returns the matrix
# set = sets the value of the matrix
# setInverse = sets the value of the inverse matrix
# getInverse = returns the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize
  invMatrix <- NULL
  #sets the value of the matrix
  set <- function(pMatrix) {
    x <<- pMatrix
    #clears the inverse matrix if existed
    invMatrix <<- NULL
  }
  #returns the matrix
  get <- function() x
  #sets the inverse matrix value
  setInverse <- function(inverse) invMatrix <<- solve
  #returns de inverse matrix value
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#This function calculates de inverse matrix from the matrix created with makeCacheMatrix,
#but first checks if the value of the inverse matrix is already stored.
#Only calculates the whole inverse matrix if it has not been calculated previously.

cacheSolve <- function(x, ...) {
  #Gets the inverse matrix value
  inverseMatrix <- x$getInverse()
  #check if it exist
  if(!is.null(inverseMatrix)) {
    #if exist return the cached value
    message("getting cached data")
    return(inverseMatrix)
  }
  #if not exist calculate
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(InverseMatrix)
  inverseMatrix
}