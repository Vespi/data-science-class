## R Programming Coursera
## Author: Vespi
## Date: 07/12/2015
##
## Heavily inspired by sample code provided for the
## cached mean function on the course website.


## Sets value of the matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      ## Function to store matrix in cache.
      set <- function(y) {
          x <<- y
          i <<- NULL
      }
      
      ## Function to retrieve stored matrx.
      get <- function() x
      
      ## Function to find the inverse of stored matrix.
      setinv <- function(solve) i <<- solve
      
      ## Function to retrieve stored inverse.
      getinv <- function() i
      
      ## Return list that stores the above values.
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Either solves for matrix inverse if not cached, or simply retrieves
## from memory otherwise.
cacheSolve <- function(x, ...) {
      ## Loads cached inverse
      i <- x$getinv()
      
      ## If it exists, load cached inverse.
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      ## Else, solve explicitly.
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
