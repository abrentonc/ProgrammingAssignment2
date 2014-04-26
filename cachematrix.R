## These functions first create a set of functions that allow me to interact with a given matrix
## then use those functions to manipulate the matrix. 


## This builds the functions in the same way as the example and puts them in a list


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


## This accepts a list created by the above function checks 
## to see if the inversion has already been cached if not it inverts the matrix and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinvers()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
}
