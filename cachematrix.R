## Functions to cache a matrix and to compute and cache its inverse. 
## Inverse is only computed if not already computed before, otherwise 
## a cached inverse is returned. This avoids costly recomputing of 
## the inverse of a matrix

## Creates a list of four functions that allow you to
## 1.) create a matrix
## 2.) return the matrix
## 3.) cache inverse of matrix
## 3.) return cached inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  # variable to cache inverse
  inv <- NULL
  
  # function to create actual matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function that just returns actial matrix
  get <- function() x
  
  # function used to cache inverse
  setinv <- function(newinv) inv <<- newinv
  
  # function that returns chached inverse
  getinv <- function() inv
  
  # returns a list of the above defined functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function calculates the inverse of the matrix if not calculated 
## before, otherwise the chached inverse is returned

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # if inverse has already been calculated the cached inverse will be returned
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # if inverse has not been calculated before, the inverse is calculated now and returned
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
