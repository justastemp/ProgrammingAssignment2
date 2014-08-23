## The follwoing functions find out if the inverse of a matrix
## is already stored; if so, the inverse matrix is simply read and returned
## instead of calculating this inverse again

## makeCacheMatrix creates a vector of functions which can get and set
## a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialise inverse matrix inv as NULL (seems to work)
        inv <- NULL
        ## This is in analogy to the cachemean example: 
        ## Save input matrix and initialise inverse to NULL in case someone
        ## accesses this method directly
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Return initial matrix x
        get <- function() x 
        ## Store inverse matrix using superassignment
        setinv <- function(invmat) inv <<- invmat
        ## Return inverse matrix inv 
        getinv <- function() inv 
        ## Create the list of all methods
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve performs the actual matrix inversion and 
## stores and/or gets he matrix and its inverse, using
## the vector of functions created through makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Look for inverse
        inv <- x$getinv()
        ## If matrix found isn't NULL, then return cached matrix and stop
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If cached matrix is NULL, get the original matrix x
        data <- x$get()
        ## Get the inverse of matrix x by using solve(x)
        inv <- solve(data, ...)
        ## Write the inverse matrix inv to the vector makeCacheMatrix
        x$setinv(inv)
        inv
}
