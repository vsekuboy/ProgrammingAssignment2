## Put comments here that give an overall description of what your
## functions do

# This program defines 2 functions - makeCacheMatrix and cacheSolve
# The makeCacheMatrix function creates a square matrix and caches its
# inverse. cacheSolve function retrieves the inverse matrix from the cache


## Write a short comment describing this function

# Below is the function "makeCacheMatrix" which creates the inverset matrix. It 
# accepts a matrix, calculates the inverset and then assigns the inverse of the
# matrix (obtained through the function solve) to the global variable InvMatrix

makeCacheMatrix <- function(Mtrx = matrix()) {
   InvMtrx <- NULL
   set <- function (y) {
     Mtrx <<- y
     InvMtrx <<- NULL
   }
   get <- function () Mtrx
   setInverse <- function (solve) InvMtrx <<-solve
   getInverse <- function () InvMtrx
   list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
# The inverse matrix is present in the global variable InvMtrx
# If this variable is null, it means the inverse matrix should be created
# for the first time. Else, it implies its presence in the cache and the value
# is retrieved directly from the cache

cacheSolve <- function(Mtrx=matrix(), ...) {
        ## Return a matrix that is the inverse of 'Mtrx'
   InvMtrx <- Mtrx$getInverse()
   if (!is.null(InvMtrx)) {
     message ("Getting matrix from Cached data....")
     return (InvMtrx)
   }
   data<-Mtrx$get()
   InvMtrx<-solve(data, ...)
   Mtrx$setInverse(InvMtrx)
   InvMtrx
}
