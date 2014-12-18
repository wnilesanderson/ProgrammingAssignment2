# Function to store and retrieve the inverse of a matrix
# Both the matrix and its inverse are preserved within the defining environment of this function,
# and are not impacted by changes in the parent environment
# The inverse is NOT computed within this function

makeCacheMatrix <- function(x = matrix()) {  
  xinv <- NULL # originally null, so that we do not retrieve an incorrect value
  
  set <- function(y) {
    # optional function, to allow the matrix to be reset
    # the main functionality of the package would remain if this optional function had not been defined
    x <<- y  
    xinv <<- NULL # resets the inverse, since the matrix was reset
  }
  
  get <- function() x 
  # retrieve the matrix x, which might be different than the matrix in the original call to makeCacheMatrix
     
  setinv <- function(inv) xinv <<- inv
  # main line code -- sets the inverse of x
  # there is no check to ensure that the inverse is valid
  
  getinv <- function() xinv
  # main line code -- retrieves the inverse of x
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# Return the inverse of a matrix that is stored in the environment of the makeCacheMatrix function
# The argument x is the name of the object that was created in the corresponding call to makeCacheMatrix
# if the inverse was present in the environment, it will be used without checking
# otherwise the inverse will be computed, and placed in the environment for later use

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    # inverse not present, so compute it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
  }

