## Put comments here that give an overall description of what your
## functions do

## similar to the example in the assignement
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## the makeCacheMatrix function. If the inverse has already been calculated
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
          message("getting cached matrix")
          return(i)
        }
        mat <- x$get()
        i <- solve(mat,...)
        x$setinv(i)
        i
  }
