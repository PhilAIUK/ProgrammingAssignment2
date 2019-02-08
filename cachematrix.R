## Put comments here that give an overall description of what your
## functions do

## These functions will show how we can retrieve data from the cache
## instead of re-calculating; which could be time consuming


## Write a short comment describing this function
## This function creates a "matrix" object and cache its inverse
## It enables to create (set) and get the matrix
## It enables to set and get tge inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                 ## Sets the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                  ## Gets the matrix
    
    setinverse <- function(inverse) {    ## Creates the inverse
        inv <<- inverse
    }
    getinverse <- function() inv         ## Gets the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## Useful to use the $ operator later

}


## Write a short comment describing this function
## This function checks if the inverse of the matrix has already been calculated and cached
## If yes, it retrieves the inverse of the matrix from the cache
## If not, it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Retrieving the cached inverse of the matrix")
        return(inv)   ## Return cached inverse
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv    ## Return calculated inverse
}
