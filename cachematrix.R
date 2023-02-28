## Assuming that the matrix is invertable the program will return the inverse of a matrix and save it in the cache.
## When a new matrix is given, it will check the cache to see if the inverse was already computed.
## If the inverse is already computed, it will return the answer.  If not, it will calculate the cache.

## Usage Example
#mymat <- makeCacheMatrix()
#mymat$set(matrix(1:4,2,2))
#cacheSolve(mymat)
#cacheSolve(mymat)  # This second time will take the inverse from the cache.


## makeCacheMatrix creates a set of four functions:
## set: sets the matrix that will be inverted
## get: retrieve the matrix
## set_inverse: calculated the inverse of the matrix
## get_inverse: retrieve the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) m <<- solve
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve will return the inverse of a matrix if one exists and save it to the cache.
## However, if the inverse was already calculated it will returne the previously calculated inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
