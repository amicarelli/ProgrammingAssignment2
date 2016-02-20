## Cache matrix operations to save computation

## Function that returns a "matrix" object and can cache matrix inversion
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


## Function to retrieve a matrix inversion value, checks first for cached version
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
