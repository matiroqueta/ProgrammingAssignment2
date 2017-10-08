## Put comments here that give an overall description of what your
## functions do

# Both functions overall work together in order to save in memory a matrix and it's inverse, and only update the inverse when the original matrix changes.
# For this purpose, they use two R programming features: lexical scoping, and using functions as objects.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  # This function creates an R object that stores a matrix and its inverse.
  # It builds a set of functions and returns the functions within a list to the parent environment.
  # Inputing an inversible matrix to the function, it returns an object that contains four functions: set(), get(), setinverse(), and getinverse(). It also includes the two data objects, x and m.
  # The matrix inverse can only be calculated if the cacheSolve() function is run on the makeCacheMatrix() object.
  # If there is already a valid inverse cached in m, whenever x is reset, the value of m cached in the memory of the object is cleared, forcing subsequent calls to cacheinverse() to recalculate the inverse rather than retrieving the wrong value from cache.
  
    m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  # This function requires an argument that is returned by makeCacheMatrix() in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.
  # As designed, cacheSolve() is required to populate or retrieve the inverse from an object of type makeCacheMatrix().
  # First it calls the getinverse() function. Then it checks whether the result is NULL. 
  # Since makeCacheMatrix() sets the cached inverse to NULL whenever a new matrix is set into the object, if the value here is not equal to NULL, we have a valid, cached inverse and can return it to the parent environment.
  # cacheSolve() is the only place where the solve() function is executed, which is why makeCacheMatrix() is incomplete without cacheSolve().
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
