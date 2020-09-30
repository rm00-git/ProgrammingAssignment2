## Put comments here that give an overall description of what your
## functions do


## makeCacheMatric involves set, get, setinv, getinv
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    ##initializing inverse Null
  set <- function(y){
    c <<-y
    inv <<- NULLls
  }
  get <- function() {x}   ##function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This will be used to get cached data

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){  ##checks whether inverse is null
    message("getting cached data")
    return(inv)  ## returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)  #calculates inverse value
  x$setInverse(inv)
  inv    ## Return a matrix that is the inverse of 'x'
}
