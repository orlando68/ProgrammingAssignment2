## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  # makeCacheMatrix creates a special "vector", which is really a list containing a function to
  # 
  # set the value of the matirx
  # get the value of the matrix
  # set the value of the matrix inverse
  # get the value of the matrix inverse
  inv_M <- NULL
  set   <- function(y) {
    x     <<- y
    inv_M <<- NULL
  }  
  get        <- function() x
  setinverse <- function(inv_Matrix) inv_M <<- inv_Matrix
  getinverse <- function() inv_M
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve  <- function(x, ...) {
  # This function calculates the inverse of the matrix of the special vector
  # Checks if the inverse matrix has been calculated.If so, it gets the inverse matrix 
  # from the cache and skips the computation. Otherwise, it calculates the inverse matrix 
  # of the data and sets the value of the inverse matrix in the cache via the setinverse function

inv_M       <- x$getinverse()
if(!is.null(inv_M)) {
  message("getting cached data")
  return(inv_M)
}
matrix_data <- x$get()
inv_M       <- solve(matrix_data, ...)
x$setinverse(inv_M)
inv_M
}
