## Put comments here that give an overall description of what your
## functions do

## 'x' is a invesible matrix ; default is an empty matrix

## this function create a object containing the matrix x (empty or not) and the inverse
## matrix (inv, by default = NULL). 

## return a list of function (set, get, setinv, getinv) which allows to set a matrix
## , to get the matrix, to set the inverse and to get the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 'x' is a special matrix created with 'makeCacheMatrix' function.

## the function first acces to the cache inverse and in the case of inverse exist
## return the inverse matrix
## If there is no inverse in the special matrix, tht function copy the matrix and
## create the inverse matrix and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
