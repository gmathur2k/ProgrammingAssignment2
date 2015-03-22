## The following two functions provide a caching solution
## to calculate and store the inverse of a matrix.
## The input matrix is stored in a list containing
## functions to set and get the matrix and its inverse
## Another function is used to access the matrix inverse
## and it calculates the inverse only once.

## The makeCacheMatrix function,  creates a special "vector", 
##  which is really a list containing a function to
##    1 set the value of the vector
##    2 get the value of the vector
##    3 set the value of the mean
##    4 get the value of the mean
## <<- operator can be used to assign a value to an object in an 
##      environment that is different from the current environment.
makeCacheMatrix <- function(mx = matrix()) {
  mxi <- NULL
  set <- function(y) {
    mx <<- y
    mxi <<- NULL
  }
  get <- function() mx
  setinverse <- function(mxInv) mxi <<- mxInv
  getinverse <- function() mxi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The cacheSolve function calculates the matrix inverse of the matrix in the  
## special "vector" created with the above function. However, it first checks 
## to see if the matrix inverse has already been calculated. If so, it gets 
## the matrix inverse  from the cache and skips the computation. 
## Otherwise, it calculates  the matrix inverse of the data and sets 
## the value of the matrix inverse in the cache via the setinverse function.

cacheSolve <- function(cmx, ...) {
  ## Return a matrix that is the inverse of 'mx'
  mxi <- cmx$getinverse()
  if(!is.null(mxi)) {
    message("getting cached data")
    return(mxi)
  }
  data <- cmx$get()
  mxi <- solve(data, ...)
  cmx$setinverse(mxi)
  mxi
}
