## These functions make a special matrix object that can cache 
## its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## initialize the inverse 
  i <- NULL
  
  ## method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## return the methods in a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## sovling for the inverse the special matrix returned by "makeCacheMatrix"
## cacheSolve returns the inverse of a matrix if it has been solved already
cacheSolve <- function(x, ...) {
  
  ## return the inverse of the matrix
  m <- x$getInverse()
  
  ## if the inverse is set, return it 
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix 
  data <- x$get()
  
  ## multiply the matrix to get the inverse 
  m <- solve(data) %*% data
  
  ## setting the inverse to the object
  x$setInverse(m)
  
  ## return the matrix
  m
  
}