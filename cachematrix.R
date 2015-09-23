# This file contains 2 functions allowing to cache the inverse of a matrix.
# It is assumed that the matrix supplied is always invertible.

# Function 'makeCacheMatrix' takes a matrix ('x') as argument
# and creates a list of functions (set, get, setinv, getinv).
# 'makeCacheMatrix' should be called through assignment ('a <- makeCacheMatrix')
# in order to access to the list's elements and call corresponding functions

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    
    # assignment to 'x', 'inv' in the parent environment
    x <<- y 
    
    # If 'x' changes, the old inverse is no longer good
    inv <<- NULL
  }
  
  get <- function() x
  
  # assignment to 'inv' in the parent environment
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


# Function 'cacheSolve' takes the list created by 'makeCacheMatrix' as argument.
# If the inverse matrix of 'x' has already been calculated (and the matrix has 
# not changed) 'cacheSolve' retrieves the inverse from the cache, otherwise
# it calculates the inverse and store its value in the cache (assigns it to inv).

cacheSolve <- function(x, ...) {
  
  # Taking the value of inv from the cache
  inv <- x$getinv()
  
  # If inv is not NULL (the invere has already been calculated)
  if(!is.null(inv)) {
    message("getting cached data")
    
    # Returne the inverse (already calculated) and exits the function cacheSolve
    return(inv)
  }
  
  # Matrix 'x' is retrieved from the cache and assigned to 'data'
  data <- x$get()
  # The inverse matrix is calculated
  inv <- solve(data, ...)
  # and stored in the cache
  x$setinv(inv)
  inv
  
}
