## Put comments here that give an overall description of what your
## functions do

## This overall program has a couple of functions that together demonstrate a
## cached implementation of matrix inverse. Specifically one function
## makeCacheMatrix will hold storage space for original and inverse matrices and## functions to set and get those matrices. The other function cacheSolve
## will return the cached inverse if the original matrix has not changed else
## compute, cache and return the inverse  otherwise.

## Write a short comment describing this function

## makeCacheMatrix will store an original matrix and have placeholder to cache
## the inverse of the original matrix. It will also expose functions to get and ## set the original matrix and the cached inverse matrix.


makeCacheMatrix <- function(x = matrix()) {

    # holds the cached inverse matrix. Initially set to NULL.
    invmat <- NULL

    # Stores original matrix and cached inverse as original matrix is changing
    setOriginal <- function(org) {

        x <<- org

        # since original matrix is newly set, reassign cached inverse to NULL
        invmat <<- NULL
    }

    # returns original matrix
    getOriginal <- function() {
        x
    }

    # store cached inverse matrix
    setInverse <- function(inv) {
        invmat <<- inv
    }

    # returns cached inverse matrix
    getInverse <- function() {
        invmat
    }

    # returns a list of all functions defined above
    list(setOriginal = setOriginal, getOriginal = getOriginal, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

## cacheSolve returns the cached Inverse matrix if the original matrix has not
## changed. Otherwise, it computes the new inverse for changed original matrix, ## stores it in the cached inverse object and returns the new inverse.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'

    # get the cached inverse
    inv <- x$getInverse()

    # checking whether inverse is already cached
    if(!is.null(inv)) {

        # return the cached inverse got in the previous step
        return(inv)
    }

    # if inverse is not cached already, get the original matrix, calculate its
    # inverse, store the newly computed inverse in the placeholder for cache and    # return it

    org <- x$getOriginal()
    inv <- solve(org)
    x$setInverse(inv)

    # return the newly computed inverse
    inv

}
