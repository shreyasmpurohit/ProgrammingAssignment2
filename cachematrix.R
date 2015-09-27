## These functions cache the process of getting the inverse
## of a square matrix

## This function makes that matrix in question

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
      list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
  )
}


## This function will return the inverse of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
      m <- x$get()
      i <- solve(m, ...)
      x$setinverse(i)
        i       
}
