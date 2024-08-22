## Put comments here that give an overall description of what your
## functions do
## There are 2 functions 
## makeCacheMatrix:
##      This function creates a special matrix with below functions:
##      (underlying structure is list of functions)
##      1. get: returns the matrix
##      2. set: changes the underlying matrix to the new matrix and resets cache
##.     3. getInverse: retrieves inverse from cache
##      4. setInverse: sets inverse into the cache
## cacheSolve:
##      This function calculates, caches and returns inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<-NULL
        }
        get <- function(){
                return (x)
        }
        setInverse <- function(i){
                inv <<- i
        }
        getInverse <- function(){
                return (inv)
        }
        p <- list(set=set, get=get, getInverse=getInverse,
                  setInverse=setInverse)
        return (p)
}


## Write a short comment describing this function
## This is a wrapper function built on cache function:
## this function saves the inverse of matrix in cache which can be 
## retrieved again thus saving time and resource
## if the underlying data is changed then it calculates the inverse, 
## stores it in cache and also returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                print('fetching inverse from cache...')
                return(inv)
        }
        data <- x$get()
        print(data)
        inv <- solve(data)
        x$setInverse(inv)
        return(inv)
}
