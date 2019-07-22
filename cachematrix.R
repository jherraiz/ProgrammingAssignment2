## Pair of functions that cache the inverse of a matrix
## In order to make it work:
## Invoke the makeCacheMatrix function to generate the list of functions
## my_functions <- makeCacheMatrix()
## Use the set function of the list to set the matrix
## mat1 <- matrix(c(0,1,0,1,0,0,1,0,1),3,3)
## my_functions$set(mat1)
## You can check that the matrix has been stores usig:
## my_functions$get()
## After that you can get the inverse twice. The first time it will be
## calculated and stored. The second time it will get it from the chache
## and skip the computation:
## cacheSolve(my_functions)
## cacheSolve(my_functions)

## This function creates a special "matrix" object that can cache its inverse.
## Generates and returns a list containing 4 different funcions that manage
## a matrix. Using those 4 functions you can set a matrix (store it), get (read) it,
## setinverse (store the inverse of the matrix if the inverse matrix is provided)
## and getinverse (read the inverse of the matrix if it has been previously stored)
makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inversematrix <<- solve
  getinverse <- function() inversematrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinverse(inversematrix)
  inversematrix
}
