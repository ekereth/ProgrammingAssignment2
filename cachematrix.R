## These two functions work together to create a special cached matrix, which can then
## have its inverse cached as well, in order to save time on future calculations.

## Makes a list of four functions that act to get, set, get the inverse of, or set the
## inverse of a cached matrix, respectively.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  #Cache the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Return the matrix value from cache
  get <- function() x
  
  #Cache the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  #Return the inverse of the matrix from cache
  getinverse <- function() i
  
  #Cache the matrix given as argument
  set(x)
  
  #Return the list of functions to access the cached matrix
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Gets the inverse of a cached matrix of the type given by the function makeCachedMatrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  #If inverse already computed, it is taken from cache and returned
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #If not, the inverse is computed, cached, and returned
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
