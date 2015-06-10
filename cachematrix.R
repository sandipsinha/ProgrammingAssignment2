## a pair of functions, first of which reverses a given matrix and the second one is more of a front loader
## which will serve up the requested object from cache if it is available else it will actually call the 
## function which renders the object. 

## Will inverse a given matrix, assuming that the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
   
}


## This is going to return the inverse of a Matrix. Since inversing Matrix is a costly operation
## it will keep the fected results in cache and will retrive it on any subsequent fetch of the same matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
