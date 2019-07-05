## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
# Function that will create a list that contains functions to handle the underlying matrix it stores.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() { x }
        calculateinverse <- function(inv) { m <<- inv }
        getinverse <- function() { m }
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             calculateinverse = calculateinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## Function that solves the equation for x if the inverse isn't already calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting calculated inverse")
                return(inv)
        }
        data <- x$get()
        inv<- solve(data)
        x$setinverse(inv)
        inv
}

