## The idea is to make use of the "solve" function and Cache the results

## This portion defines the two functions x and s, which will be used by CacheMatrix function below.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
  x <<- y
  s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This portion uses the data and functions from the above to check if the
## inverse of the matrix was calculated and returns the value from cache. 

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
    }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s           ## Return a matrix that is the inverse of 'x'
}
