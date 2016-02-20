## Cache matrices operations to save computation

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat = NULL
  inv = NULL
  # set
  set <- function(y){
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinv(inv)
  inv
}
