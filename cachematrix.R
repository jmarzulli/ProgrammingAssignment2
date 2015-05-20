## These functions cache the inverse of a matrix to minimize the need to compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse. The function 
## includes four sub functions that can "set" (define) and "get" (return) the matrix itself 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## creates a function that redefines the value x
    x <<- y
    m <<- NULL
  }
  get <- function() x ## creates a function that returns the value of x 
  setinverse <- function(inverse) m <<- inverse ## creates a function that redefines the value of m
  getinverse <- function() m ## creates a function that that returns the value of m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## creates a list of 4 functions 
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## defines m as equal to the getinverse function from the prior equation
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## if m has previously been cached, returns the value of m (the matrix inversion)
  }
  data <- x$get() ## if m has not been cached, sets data equal to the original matrix
  m <- solve(data, ...) ## inverts the matrix
  x$setinverse(m) ## saves the value of the inverted matrix
  m ## returns the inverted matrix
}