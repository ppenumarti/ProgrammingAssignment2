#### Put comments here that give an overall description of what your functions do

## R-Programming allows object orientation - we can encapsulate functionalities into any object.
## R-programming allows functions as first class objects.
## "Closures" in Functional Programming is another idiom that can be associated in your thoughts.
## Below script lists two functions 1) makeCacheMatrix and 2) cacheSolve
## makeCacheMatrix associates a matrix and it's inverse as a pair.
## cacheSolve will check prior-existense of inverse and computes fresh ONLY if necessary.
## Usage of <-- operator is exemplified to prove scoping rules across two R-Ojects
## This script uses a library method solve() for computing inverse of a matrix.
## To use: One can initiate a matrix into makeCacheMatrix and obtain it's inverse by using cacheSolve

#### Write a short comment describing this function

## makeCacheMatrix initiats a cache value to NULL, for repeated assignments later.
## Whenever a new matrix is created by set method, the inverese is reset to NULL.
## To test this function alone, follow the below sequence of steps:
## 1) Construct an R-object myMat by calling makeCacheMatrix()
## 2) Set a square matrix to your created object, myMat$set(<<insert your test matrix here>>)
## 3) Optionally you can verify the matrix by calling a get method on your object.
## 4) Create inverse of your matrix by calling myMat.setInverse
## 5) Obtain inverse of your matrix by calling myMat.getInverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- solve(x)
    getInverse <- function() inv
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


#### Write a short comment describing this function
## cacheSolve is capable of checking existing pair of matrix and it's inverse before every fresh compute.
## To test, call this function by providing an R-Object created by above method makeCacheMatrix
## Until a new matrix is set on makeCacheMatrix, the message "getting cached data" appears in output.
## If you change matrix by mutation on makeCacheMatrix, the message disappears.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
