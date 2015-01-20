## Those two function aim to return the invert of a square invertible matrix 
## with a low usage of memory by putting variables in cache.

## makeCacheMatrix is use to cache setters and getters in order to manipulate 
## the square invertible matrix passed as agurment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  ## Create an empty Variable
  Set <- function(x1) { ## Setter for the cached matrix
    x <<- x1 
    i <<- NULL
  }
  Get <- function() x ## Getter for the cached matrix
  SetInvert <- function(invert) i <<- invert ## Setter for a cached solved matrix
  GetInvert <- function() i ## Getter for the cached solved matrix
  ## Create a list of items with Getters and Setters
  list(Set = Set,Get = Get,SetInvert = SetInvert,GetInvert = GetInvert)
}


## cacheSolve return the invert of a square invertible matrix cached with
## the function makeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## Get the cached invert of the matrix and test if it is NULL 
  so <- x$GetInvert()
  if(!is.null(so)){
    ## advise the user that the invert has been retrive from the cache
    message("getting data from cache")
    ## retrun the cached invert
    return(so)
  }
  ## Get the matrix into a local variable
  matrix <- x$Get()
  ## Compute the invert of the matrix with the solve function
  solve <- solve(matrix)
  ## Cache the invert in order to retrive it with low memory usage.
  x$SetInvert(solve)
  ## Return the invert
  solve
}