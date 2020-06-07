## The function creates an matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(m){
    x <<- m
    n <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) n <<- inverse
  getInverse <- function() n 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function calculates the inverse of the matrix returned by makeCacheMatrix above. CacheSolve retrieves the inverse of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
  if(!is.null(n)){
    message("getting cached data")
    return(n)
  }
  mat <- x$get()
  n <- solve(mat,...)
  x$setInverse(n)
  n
}
