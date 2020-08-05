## The following functions compute and cache the inverse of a squared matrix
## Assumption: the input matrix is assumed to be a square matrix capable of inversion

## makeCacheMatrix creates the matrix for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  iMatrix <- NULL

  set <- function(y) {
    
    x <<- y
    
    iMatrix <<- NULL
    
  }
  
  get <- function() x
  
  setSolve <- function(solve) iMatrix <<- solve
  
  getSolve <- function() iMatrix
  
  list(set = set, get = get,
       
       setSolve = setSolve,
       
       getSolve = getSolve)

}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  iMatrix <- x$getSolve()
  
  if(!is.null(iMatrix)) {       #if the inverse already made, simple return it
    message("getting cached data")
    return(iMatrix)
  }
  data <- x$get()
  iMatrix <- solve(data, ...)
  x$setSolve(iMatrix)
  iMatrix
}
