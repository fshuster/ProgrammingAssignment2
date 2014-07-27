####
## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that will store a copy (aka cache) 
## of the inverse of a matrix for later use in a vector of 4 values:
##
## 1. Set the Value of the vector
## 2. Get the value of the vector
## 3. Set the Inverted Matrix
## 4. Get the Inverted Matrix
####

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve is a function that first checks to see if there is a 
## stored inverse for the matrix.  If the inverse is already stored, 
## it returns the cached inverse. If the inverse is not cached, then 
## cacheSolve calculates the inverse matrix and stores it in setInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
