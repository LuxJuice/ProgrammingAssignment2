## These functions uses the <<- operator to set variables that are
## in the parent scope of the function. By using lexical scoping this
## way, we can "cache" complex calculations that are time consuming

## Sample usage:

# > myMatrix <- matrix(c(1, 2, 3, 4), nrow=2)
# > cacheMatrix <- makeCacheMatrix(myMatrix)
# > cacheMatrix$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(cacheMatrix)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cacheMatrix)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## This function replaces the standard matrix() function in R

## Notice use of the <<- operator, which manipulates symbols one
## level up in the evironment heirarchy

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## This function takes advantage of the <<- used in the previous function

## If no inverse been calculated, then the call to x$setInv(m) is able to 
## alter the makeCacheMatrix object and store the inverse for future lookup

cacheSolve <- function(x, ...) {
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
  
}
