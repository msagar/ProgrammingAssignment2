
# Script to calculate inverse of a invertible matrix with optimization to allow caching of already calculated inverse of the matrix.


# Function that will create a list that contains functions to handle the underlying matrix it stores.

# Example how to use the functions below:
#
# bigmatrix = matrix(rnorm(10000),nrow = 100, ncol = 100)
# cacheSolve(makeCacheMatrix(bigmatrix))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() { x }
        setinverse <- function(inv) { m <<- inv }
        getinverse <- function() { m }
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)

}



## Function that solves the equation for x if the inverse isn't already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting calculated inverse")
                return(inv)
        }
        data <- x$getmatrix()
        inv<- solve(data)
        x$setinverse(inv)
        inv
}

