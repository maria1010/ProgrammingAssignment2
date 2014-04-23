## Computing the inverse of a matrix is an expensive task.
## These functions will provide a way to cache the results
## of previously calculated inverse values. This way, they
## don't need to be re-calculated again, if it has been
## calculcated previously.

## The makeCacheMatrix is a function that provides methods
## for caching the original/given matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # the inverse of x matrix
  inverse <- NULL
  # setter for matrix x; reset inverse to null
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # getter for matrix x
  get <- function() x
  # setter for inverse of x
  setInverse <- function(solve) inverse <<- solve
  # getter for inverse of x
  getInverse <- function() inverse
  # return the name of this method
  getFunctionName <- function() return(c("makeCacheMatrix"))
  list(set = set, get = get, 
       getInverse = getInverse, 
       setInverse = setInverse,
       getFunctionName = getFunctionName)
}

## The cacheSolve function calculates the inverse of the
## given matrix, if it does not already exist, caches it,
## and returns the value of the computed inverse. Otherwise, 
## it simply returns the previously computed and cached 
## inverse value.

cacheSolve <- function(x, ...) {
  ## check if x is valid, if not, return NULL
  if (!isValidDataForCacheSolve(x)) {
    return(NULL)
  }
  
  ## Try to get the inverse of x from cache
  inverse <- x$getInverse()
  ## If it exists, let the user know and
  ## return the inverse value
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## get the data
  data <- x$get()  
  ## compute the inverse of x using solve
  inverse <- solve(data, ...)
  ## cache the computed inverse value
  x$setInverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}

## The isValidDataForCacheSolve is a function that checks the
## value of the parameter. To be valid, x must be created using
## the makecacheMatrix function; x must be a matrix; and
## x must be a square matrix. This function returns true, if
## all conditions are met. Otherwise, it returns false.

isValidDataForCacheSolve <- function(x) {
  ## check if x has been created using makeCacheMatrix
  tryCatch ({
    n1 <- x$getFunctionName()
    n2 <- c("makeCacheMatrix")
    if (!(n1 == n2))
      return(isValidDataForCacheSolveErrorFunction())
  }, error = function(e) { 
    return(isValidDataForCacheSolveErrorFunction()) 
  })
  ## get the data
  data <- x$get()
  ## check if the data is matrix
  if (!is.matrix(data)) {
    message("Error: data provided is not a matrix")
    return(FALSE)
  } 
  ## check if the data is a square matrix
  else if (!(ncol(data) == nrow(data))) {
    message("Error: data provided is not a square matrix")
    return(FALSE)
  }
  return(TRUE)
}

## The isValidDataForCacheSolveErrorFunction is a function
## used by the isValidDataForCacheSolve to display an error
## message when the data provided is not created by 
## makeCacheMatrix. It returns a false value.
isValidDataForCacheSolveErrorFunction = function() {
  message("Error: data provided was not created using makeCacheMatrix")
  return(FALSE)  
}