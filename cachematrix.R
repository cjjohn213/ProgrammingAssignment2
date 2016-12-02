## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function Creates a Special Matrix that can set a cache value of
## it's inverse with the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    ## On initial object creation set inversed value to null
    ci <- NULL
    ## if function was used prior, reset value
    reset <- function(y) {
        message("called once before")
        x <<- y
        ci <<- NULL
    }
    ## when called return inverse
    getInverse <- function() ci
    ## when called returns value of matrix
    getMatrix <- function() x
    ## when called sets value of ci
    setCi <- function(inverse) ci <<- inverse
    ##name items in list for easier function calls
    list (reset = reset,
          getInverse = getInverse,
          getMatrix = getMatrix,
          setCi = setCi)
}


## Write a short comment describing this function
## (writing step by step inside function mainly to help me create and follow)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ##check to see if Cached value of Special Matrix exists
    ci <- x$getInverse()
    ## if value has been calculated
    if (!is.null(ci)) {
        message("returning cached value")
        return(ci)
    }
    ## ELSE set inverse and change cached inverse value in Special Matrix
    message("going to create inverse")
    ## get value of Special Matrix
    thisMatrix <- x$getMatrix()
    ## calculate inverse
    ci <- solve(thisMatrix)
    ## set value of cached inverse in Special Matrix
    x$setCi(ci)
    ci
}
