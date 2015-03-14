## Author: Patrick Hew
## Date: 14/3/2015
## This R code is for R Programming Assignment2

## This function accepts a matrix object X as an argument
## The first function, makeCacheMatrix creates a special "matrix" and contains a list to
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## Checks if the inverse is cached already or not
## if it's not cached yet, call makeCacheMatrix
## otherwise, return the cached value
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setInverse(m)
  m
}