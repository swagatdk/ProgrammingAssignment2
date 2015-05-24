## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The below pair of functions cache the inverse of a matrix for repeat use

## The below function creates special class of type list containing a matrix 
## and its inverse with 2 pair of getter and setter methods
makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL  
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  get <- function() x
  setInv <- function(inverse = matrix()) inv.x <<- inverse
  getInv <- function() inv.x
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The below function takes the list created in above method as an argument and 
## returns the inverse of the original matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  ## First try to get it from cache
  inv.x <- x$getInv()
  if (!is.null(inv.x )) {
    message("Returning from cache")
    return(inv.x)
  }
  ## If not available in cache, then get the original matrix first
  ## Solve it, store the inverse in cache and then return the inverse
  data <- x$get()
  inv.x <- solve(data)
  x$setInv(inv.x)
  inv.x  
}
