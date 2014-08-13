
# The encoding of the script is CP936
# R version 3.1.1 (2014-07-10) -- "Sock it to Me"
# 20140813 uhuruqingcheng

# Couresra-[R Programming][rprog-006][Programming Assignment 2]


## This script including a pair of functions that will cache the inverse of 
# a matrix.

## Usage Example
# m <- matrix(runif(n=16), 4, 4) # create a random matrix
# cm <- makeCacheMatrix(m) # create the cached matrix
# cacheSolve(cm) # solve for inverse (any subsequent call will return the cached value)

## This function creates a special "matrix" object that can cache its inverse.
# This special "matrix" is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matdix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inv <- NULL
  
  # set and get matrix data with initially empty cache
  set <- function(y){
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  
  # set (i.e. cache) and get inverse
  setinverse <- function(inverse) matrix_inv <<- inverse
  getinverse <- function() matrix_inv
  
  # return list of function
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.
cacheSolve <- function(x, ...) {
  
  # get the inverse and if cached (i.e. non-null) return that value
  matrix_inv <- x$getinverse()
  if(!is.null(matrix_inv)){
    message("getting cached data")
    return(matrix_inv)
  }
  
  # calculate the inverse
  data <- x$get()
  matrix_inv <- solve(data, ...)
  
  # cache the inverse
  x$setinverse(matrix_inv)
  
  # return the inverse
  matrix_inv
}

