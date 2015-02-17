## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache



## The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) i <<- solve
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}



## The cacheSolve function calculates the inverse of the special "matrix" with the makeCacheMatrix function.
## However, it first checks to see if the inverse of the matrix already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the 
## setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setInverse(i)
  
  i
}