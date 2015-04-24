## Dan Stegman
## Coursera course Programming with R
## Programming assignment #2
## April 21st, 2015

## Per the programming assignment description, these functions will cache a
## passed matrix, and solve the cached matrix as an inverted matrix
## the reasons for this is you can do all the cacheing in advance, and if
## two matrices match, you don't have to calculate it's inverse again, you
## can just use the cached version

## makeCacheMatrix will cache a passed matrix x, so that it will not have to 
## be solved again in the future, saving processing time

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        list(
                ## set takes matrix x and "sets" it in a special memory object
                set = function(y) {
                        x <<- y
                        invX <<- NULL
                },
                ## get returns the matrix x introduced by set
                get = function() {
                        x
                },
                ## setSolve creates the inverse of x
                setSolve = function(m) {
                        invX <<- m
                },
                ## getSolve returns the solution m (inverse of x)
                getSolve = function() {
                        invX
                }
        )
}


## cacheSolve will check to see if x has a solution already
## if the inverse has already been solved, return the cached solution

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        ## First, get the solution from getSolve
        m <- x$getSolve()
        
        ## Check to see if it is NULL.  If it isn't, returned the cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If m was NULL, solve and cache the inverse
        data <- x$get()
        m <- solve(data)
        x$setSolve(m)
        m
}
