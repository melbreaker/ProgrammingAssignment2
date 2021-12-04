## Caching the inverse of a Matrix

## This function create a matrix object as well as cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <-NULL
  }
  
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function compute the inverse of the matrix from makeCacheMatrix. 
## If the inverse is cached, it will retrieve the inverse from the cache. 
## If the inverse is not cached, it will calculated an inverse and cache it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
