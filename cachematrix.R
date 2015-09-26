## Function cacheSolve returns the inverse matrix of a matrix. This value can be either returned from cache 
## or be calculated. cacheSolve use another function makeCacheMatrix as an argument.


## function makeCacheMatrix starts with destroying object m in the local environment and makes a list of 4 functions: 
## get - gets an object x indicated in the function argument, x is located in the environment from where the function 
## is called
## set - change the value of object x and destroy the object m in the environment from where the function is called 
## setInvMatrix - assigns the value of the function argument to an object m located in the environment from where
## the function is called
## getInvMatrix - gets the value stored by setInvMatrix (NB! to prevent getting m from the previous run, 
## m is reset to NULL at the beginning)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
        x <<- y
        m <<- NULL
        }
  get <- function() x
  setInvMatrix <- function(invmatrix) m <<- invmatrix
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## cacheSolve applies functions made by makeCacheMatrix:
## - gets the m value stored by setInvMatrix (see makeCacheMatrix); 
## - if m exists in the cacheSolve local environment (not NULL), the inverse matrix comes from cache with a message
## - otherwise the function gets matrix mentioned in the makeCacheMatrix argument,
## calculates the inverse matrix and assigns the results to object m,
## stores calculation results,
## and retuns the final inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMatrix(m)
        m
}
