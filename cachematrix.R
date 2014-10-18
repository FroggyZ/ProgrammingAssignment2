################################################################################
##                 ASSIGNMENT 2 - MATRIX INVERSION IN CACHE                   ##
################################################################################

## The first function "makeCacheMatrix" creates a special "matrix", which is 
## a list containing four (4) functions:
##      setMx: to set the value of the Matrix
##      getMX: to get the value of the Matrix
##      setinverse: to set the value of the Inversed Matrix
##      getinverse: to get the value of the Inversed Matrix

## function "makeCacheMatrix" takes a matrix "A" as an argument
makeCacheMatrix <- function(A = matrix()) {
        
        ## First, it sets locally "invA" to NULL
        invA <- NULL
        
        ## function "setMX" nested in "makeCacheMatrix"
        ## creates two (2) variables with the super assignment operator
        ## to update values of matrices "A" and "invA" 
        ## (stored outside the "makeCacheMatrix" function environment)
        setMx  <- function(Y) {
                A    <<- Y
                invA <<- NULL
        }
        
        ## function "getMx" nested in makeCacheMatrix
        ## gets the value of the "A" matrix
        getMx <- function() A
        
        ## function "setinverse" nested in makeCacheMatrix
        ## will do the matrix inversion with solve function
        ## and update - with its results - the value of "invA"
        ## (stored outside the "makeCacheMatrix" function environment)
        setinverse <- function(solve) invA <<- solve
        
        ## function "getinverse" nested in makeCacheMatrix
        ## gets the value of the "invA" matrix
        getinverse <- function() invA
        
        ## function "makeCacheMatrix" creates a list of these
        ## four (4) functions, which can also be used to manipulate
        ## each the matrix "A" items
        list(setMx = setMx, 
             getMx = getMx, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function "cacheSolve" calculates the inverse of the special 
## "matrix" created with the above function. However, it first checks to see
## if the matrix has already been inversed. If so, it gets the inversed matrix
## from the cache and skips the computation. Otherwise, it computes the inverse
## the matrix and sets the value of invA in the cache via the setinverse
## function.

## function "cacheSolve" takes a matrix "A" as an argument
cacheSolve <- function(A, ...) {
        
        ## First, recover what is in "invA"
        invA <- A$getinverse()
        
        ## If "invA" is not NULL, then it exists in caching memory and
        ## it can be recovered quiting "cacheSolve"
        if(!is.null(invA)) {
                message("getting cached data")
                return(invA)
        } 
        
        ## Else if "invA" is NULL, inverse of "A" has not been
        ## computed before ! It's thus time to proceed with
        ## matrix inversion using functions defined in "makeCacheMatrix"
        else {
                message("no cached data ==> inverse matrix !")
        }
        
        
        ## the lines below will execute ONLY when "invA" is NULL
        ## (they could have been written in the "else" braces)
        
        ## get the "A" matrix to invert (which has been stored
        ## outside the "cacheSolve" function environment, when 
        ## "makeCacheMatrix" was executed or its sub-function "setMX")
        data <- A$getMx()
        
        ## proceed with its inversion ("invA" is local)
        invA <- solve(data, ...)
        
        ## now, store results of the local "invA" computed above 
        ## outside the "cacheSolve" function environment
        A$setinverse(invA)
        
        ## print local invA
        invA
}