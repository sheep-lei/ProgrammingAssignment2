## Put comments here that give an overall description of what your
## functions do
#The makeCacheMatrix function is a function that set the rule of the cache of 
#a matrix and its inverse. The cacheSolve function uses the rule of makeCacheMatrix
#function to get the matrix and calculate its inverse.
#


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getInverse <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setMatrix(m)
  m
}

