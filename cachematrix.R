## Functions to calculate inverse of matrices and cache them

## Creates a vector of functions that retrieve and cache matrices to reduce computation resources

makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL ##Inverse of supplied matrix
  
  ##Cache supplied matrix
  setmatrix <- function(y) {
    mtx <<- y
    inv <<- NULL
  }
  
  ##Retrieve cached matrix 
  getmatrix <- function() mtx
  
  ##Cache inverse of supplied matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ##Retrieve cached inverse of supplied matrix
  getinverse <- function() inv
  
  ##Returns list of functions to be called by cacheSolve and user
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Calculates inverse of cached matrix using makeCacheMatrix vector functions and caches results.
## Retrieves solution if inverse has already been calculated.

cacheSolve <- function(x) {
  
  ##Check for and retun already calculated inverse matrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Using cached inverse matrix!")
    return(inv)
  }
  
  ##Retrive matrix, calculates inverse, cache and return solution
  mtx <- x$getmatrix()
  inv <- solve(mtx)
  x$setinverse(inv)
  inv
  
  }
