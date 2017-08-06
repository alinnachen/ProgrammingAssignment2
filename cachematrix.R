## Coursera R Programming Course- Assignment 2
## Author: Hai Wen CHEN
## Acknowledgments:Part of the comments and the code originated from the course material of the "Coursera 
## John Hopkins University- R Programming" course taught by  Roger D. Peng, PhD,  Jeff Leek, PhD,Brian Caffo, PhD.
## 06/08/2017


## This R script contains two functions.
## 1. makeCachematrix: This function creates a special "matrix" object 
##    that can cache (store) its inverse. 
## 2. cacheSolve : This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been calculated 
##    (and the matrix has not changed), then the cachesolve should retrieve the inverse 
##    from the cache.

## this function, makeCacheMatrix, creates a matrix object, x, and a 'list' of functions 
## that can respectively
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function, cacheSolve, calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
