## My goal is to create 2 functions that work together to cache the inverse of a matrix.

## 1 function takes a matrix as its argment and creates variables and four functions 
## within it: set, get, setInverse, and getInverse. The result of the function can be described 
## as an S3 environment that includes not only the functions in its list, but also any 
## variables in memory of the function where the list was created.

## The purpose of the first function is to be able to cache the inverse of the matrix
## in case it is needed again. The 1st function makes its environment available 
## to other functions using the <<- operator

## The 2nd function uses as its argument the R object returned by the 1st function and 
## must use the $ operator to utilize the functions defined within that 1st function



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(source) m <<- source
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
