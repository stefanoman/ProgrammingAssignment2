## This code contains two functions for Programming Assignment 2.  The first
## function, "makeCacheMatrix" accepts a matrix and is able to cache the inverse of the matrix.  
## Note "makeCacheMatrix does NOT comput the inverse of the matrix.  This is done in the second 
## function, "cacheSolve".  This function checks if there is already a solved matrix inverse
## in memory, and if so returns the cached solution.  If there is no existing matrix inverse in memory, or 
## a new one needs to be calculated because the main matrix was changed.

## makeCacheMatrix accepts a matrix and is able to cache the inverse of the matrix.  
## Note "makeCacheMatrix does NOT comput the inverse of the matrix. 



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                             ## if m is NULL, a new matrix inverse needs to be computed
  ## The set function reassigns the matrix in "x"
  set <- function(y) {   
    message("I'm in the set function")
    x <<- y
    m <<- NULL  ## Return "m" to NULL to signal the inverse needs to be computed
    
  }
  get <- function() x                       ##XXX$get returns the value of the cached matrix
  setInv <- function(solve) m <<- solve     ## XXX$set sets the cached inverse with new values
  getInv <- function() m                    #XXX$getInv returns the value of the inverse. If not cached, NULL
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve checks if there is already a solved matrix inverse
## in memory, and if so returns the cached solution.  If there is no existing matrix inverse in memory, or 
## a new one needs to be calculated because the main matrix was changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Check if the inverse exists by getting the Inverse out of memory and seeing if it is valid or NULL
  m <- x$getInv()                   
  if(!is.null(m)) {
    message("getting cached data")                  ## If "m" not NULL, fetch the inverse from cache
    return(m)
  }
  
  ## If "m" is NULL, need to compute the matrix inverse
 
  data <- x$get()                                    # Get the current matrix
  message("I'm calculating the result")
  
  # solve for the inverse and store in "m"
  m <- solve(data, ...)
  
  #set the cached inverse of "x" with the new solution
  x$setInv(m)
  m
  
  
  }


## TESTS FOR THE FUNCTIONS.
## The functions can be tested with the following inputs.....

#  aaa <- makeCacheMatrix(matrix(c(1:4),nrow=2,ncol=2))
#  aaa$get()
# aaa$getInv()
# cacheSolve((aaa))
# aaa$set(matrix(c(3:6),nrow=2,ncol=2))


