################################################################################
##                 ASSIGNMENT 2 - MATRIX INVERSION IN CACHE                   ##
################################################################################

## The first function "makeCacheMatrix" creates a special "list" vector, 
## containing four (4) functions:
##      setMx -------> to set the value of the original Matrix to be inverted
##      getMX -------> to get the value of the original Matrix to be inverted
##      setinverse --> to set the value of the Inversed Matrix
##      getinverse --> to get the value of the Inversed Matrix

## Function "makeCacheMatrix" takes, as argument, a matrix "A":
makeCacheMatrix <- function(A = matrix()) {
        
        ## First, it sets locally "invA" to NULL
        invA <- NULL
        
        ## Function "setMX" nested in "makeCacheMatrix":
        ## creates two (2) variables with the super assignment operator
        ## to update values of matrices "A" and "invA" 
        ## (stored outside the "makeCacheMatrix" function environment)
        setMx  <- function(Y) {
                A    <<- Y
                invA <<- NULL
        }
        
        ## Function "getMx" nested in makeCacheMatrix:
        ## gets the value of the "A" matrix
        getMx <- function() A
        
        ## Function "setinverse" nested in makeCacheMatrix:
        ## will do the matrix inversion with solve function
        ## and update - with its results - the value of "invA"
        ## (stored outside the "makeCacheMatrix" function environment)
        setinverse <- function(solve) invA <<- solve
        
        ## Function "getinverse" nested in makeCacheMatrix
        ## gets the value of the "invA" matrix
        getinverse <- function() invA
        
        ## Function "makeCacheMatrix" is a list of these four (4) functions,
        ## which can also be used individualy (as with regular list of elements)
        ## to manipulate each items of matrix "A"
        list(setMx = setMx, 
             getMx = getMx, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function "cacheSolve" calculates the inverse of the original 
## matrix. However, first it checks if the matrix has already been inversed.
## If so, it gets the inversed matrix from the cache and skips computations.
## Otherwise, it computes the inverse of the matrix and sets the value of invA
## in the cache via the "setinverse" function.

## Function "cacheSolve" takes, as argument, a matrix "A":
cacheSolve <- function(A, ...) {
        
        ## First, recover what is in "invA"
        invA <- A$getinverse()
        
        ## If "invA" is not NULL, then it exists in caching memory. It can 
        ## be recovered (A as already been inversed) and "cacheSolve" 
        ## can be quit.
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
        
        
        ## The lines below will execute ONLY when "invA" is NULL
        ## (they could have been written in the "else" braces as well):
        
        ## 1) get the "A" matrix to invert (which has been stored
        ## outside the "cacheSolve" function environment, when 
        ## "makeCacheMatrix" was executed or its sub-function "setMX"):
        data <- A$getMx()
        
        ## 2) proceed with its inversion ("invA" is local):
        invA <- solve(data, ...)
        
        ## 3) Now, store results of the local "invA" computed above 
        ## outside the "cacheSolve" function environment:
        A$setinverse(invA)
        
        ## 4)print local invA:
        invA
}