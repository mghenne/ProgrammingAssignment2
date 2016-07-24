## the following 2 functions cache the inverse of a matrix (so it doesn't need to be 
## re-calculated each time)

## the first function creates a special 'matrix' that sets the 
## value of the matrix, gets the value of a matrix, sets and gets 
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the second function computes the inverse of the matrix
## created above. First it will check if the inverse is already 
## created, if it is it will get it from the cache, 
## otherwise it computes it and sets it into the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
