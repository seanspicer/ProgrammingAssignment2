#
# Author: Sean Spicer (sean.spicer@gmail.com)
# Date: 17 May 2015
#

# NOTE - I've started replacing the '<-' with '=' in 
# my R code as its clearer to me, and I see no good reason
# to maintain S-PLUS Compatibility.
#

## This function creates a 'special' data structure
## containing the matrix and a slot for a cached
## inverse of the matrix.
makeCacheMatrix = function(x = matrix()) {
  
  # m = the inverse of the cached data.
  m = NULL
  
  # setter: Store the original matrix, null the inverse
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  
  # getter: return the stored matrix
  get = function() x
  
  # setter: Store the inverse matrix
  setinverse = function(inverse) m <<- inverse
  
  # getter: Return the inverse matrix
  getinverse = function() m
  
  # Create the list of functions and return.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function Return a matrix that is the inverse of 'x'
## if the inverse has already been cached return that instead.
## 'x' must be the special matrix returned from makeCacheMatrix(x)
cacheSolve = function(x, ...) {
  
  # Get the inverse, check if null.  If not null, 
  # return the cached inverse.
  m = x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # There was no cached inverse, get the data
  data = x$get()
  
  # Solve for the inverse
  m = solve(data, ...)
  
  # Set the inverse in the cache
  x$setinverse(m)
  
  # Return the inverse
  m
}