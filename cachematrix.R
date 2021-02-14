## This program will solve for and cache the inverse of matrix. If the inverse 
## has already been cached, a message will let us know this. 

## m1 and n2 are test cases used to make sure the functions work as expected
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow=2, ncol=2)
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow=2, ncol=2)

## makeCacheMatrix builds a set of functions to set and get the inverse and 
## returns them as a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { 
    x <<- y  ## assigns input arg to the x objext in parent env.
    inv <<- NULL  ## clears any previously cached inverse matrix
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve 
  getinverse <- function() inv 
  list(set = set, get = get, ## assigns each function as an element in a list
                              ## so that they can be retrieved in cacheSolve
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function actually solves for the inverse of the matrix by first 
## checking to make sure the solution has not been cached previously, then 
## solving for the inverse of the matrix and returning the answer and caching 
## the solution for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) { ## if this inverse has already been cached, then this
                      ## message will appear along with the inverse
    message("getting cached data for inverse of matrix")
    return(inv)
  }
  ## else the inverse is solved for, cached, and returned
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
