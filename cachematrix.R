##   x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

##creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <-function(y) {
    x <<-y
    inv <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<-inverse
  
  getinverse <- function() inv
  list (set = set, get = get, setinverse = setinverse, getinverse=getinverse)

}


## computes the inverse of the "matrix" returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inv <- x$getinverse()
  
  ## if the inverse has already been calculated
  
  if (!is.null(inv)) {
    ## get it from the cache and skip the computation
    message ("getting cached data")
    return(inv)
  }
  ## otherwise, calculate the inverse 
  data <- x$get()
  inv <- solve(data,...)
  ## sets the value of the inverse in the cache via the setinv function.  
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
