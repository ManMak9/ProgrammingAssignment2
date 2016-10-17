## Two functions to create and cache a matrix and its inverse, makeCacheMatrix caches the matrices
## cacheSolve calculates the inverse of a matrix or simply returns the cached value rather than calculate it again
## First create a list object by calling makeCasheMatrix and pass it a matrix, 
## then call casheSolve passing it the list object each time you want the invese of the matrix.
## If the values of the matrix are changed, you need to call the makeCacheMatrix again.


## Creates two cached matrices; x and its inverse m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of a matrix. It will first check if the invese is cached
## if it is, it will return the cached value (not null)
## if it is not cached, the fuction will calculate the invese and then store it in cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}