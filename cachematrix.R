## makeCacheMatrix function creates the matrix and caches the inverse when calculated
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(y_inv) x_inv <<- y_inv
  getinv <- function() x_inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function returns the inverse of the matrix from cache, if not it calculates it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)){
    message("Getting cache data.")
    return (x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinv(x_inv)
  x_inv
}
