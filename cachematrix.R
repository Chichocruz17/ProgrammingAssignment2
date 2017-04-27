## In this file you will find two functions, two cache the the Inverse of a matrix. 
##In this function the creation of a special matrix with the ability to cache its inverse. 
##makeCacheMatrix the name of the function, were X is a so called matrix, inv is a list with zero lenght to provide an undefined value, and set is a function "y" then x<<- y operator is used for assigning to variables in the parent same for inv <<- null, then we get the function() x and setInverse is calling a function to solve Matrix and getInverse is extracting that function. 

##CacheSolve is created to compute the outcome of makeCacheMatrix and all it does is returns the inverse of "x"


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse,
  getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
 if(!is.null(inv)) {
   message("getting cached data")
    return(inv)
   }
  data <- x$get()
  inv <- Solve(data)
  x$setInverse(inv)
  inv
}