## Functions developed below allow saving time of computation in inverse matrix operations. It can be done using lexical scoping which
## means store a special matrix object in another environment and cache the inverse matrix values when is needed rather than recomputed.


## Creates a special matrix object in a new environment that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function looks in the environment of the particular function above and computes the inverse of the special matrix, stores it in that environment and give the result. If the inverse has already been calculated and the matrix has not changed the cachesolve return the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
