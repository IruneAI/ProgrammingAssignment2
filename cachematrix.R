# The following functions provides the benefits of caching the inverse of a matrix
# rather than compute it repeteadly. 

#Following the "mean" example approach,
# makeCacheMatrix creates a list containing a function to
# 1. set(): set the value of the matrix 
# 2. get(): get the value of the matrix 
# 3. setinverse(): set the value of inverse of the matrix
# 4. getinverse(): get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  #inialize inverse matrix
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) inverse <<- matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# This function computes the inverse a matrix. 
# In order to achieve the best performance, it will be checked if the inverse was previously calculated. 
# If so, the cached inverse will be retrieved. 
# If not, it will be calculated and returned. 

# Invertible matrix is assumed (square matrix) as this function parameter.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  #check wether the matrix has already been calculated 
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  #else get the matrix
  mat<- x$get()
  #calculate the inverse of the matrix
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}