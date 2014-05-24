## The functions below create a matrix, calculate its inverse, cache the inverse
## and retrieves the cached value. The idea is to spare the system of
## time-consuming operations. (sorry for my english, I'm not native speaker)

## This first function set the values of the matrix, get the value of the
## matrix, calculates the inverse and get the value of the inverse.

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


## This second function checks if the inverse has already been calculated.
## If the inverse has been calculated, the function retrieves the cached value,
## if not, it calculates it and store it in the cache.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m        ## Return a matrix that is the inverse of 'x'
}
