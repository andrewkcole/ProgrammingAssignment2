
## This code contains two main functions, makeCacheMatrix is a function object that contains two internal matrix objects xMatrix and xInverse that can be accessed with the functions getMatrix and getInverse.

## cacheSolve is a function that takes an object created by the makeCacheMatrix function and either returns the cached xInverse value if it exists or calculates the inverse of the matrix, returns it and stores (caches) it inside the xInverse container.


makeCacheMatrix <- function(x = matrix()) {
    ## contains two objects
        ## xMatrix - simple original matrix
        ## xInverse - inverse of xMatrix (or null if not yet computed)

    xMatrix <- x
    xInverse <- NULL
    
    ## set function sets internal matrix 'xMatrix' to the new value 'newMatrix'
    ## and resets the cached inverse 'xInverse' to NULL
    setMatrix <- function(newMatrix) {
        xMatrix <<- newMatrix
        xInverse <<- NULL
    }
    ## get function returns internal matrix 'x'
    getMatrix <- function() xMatrix
    
    ## setInverse and getInverse **should be private functions** 
    setInverse <- function(newInverse) xInverse <<- newInverse
    getInverse <- function() xInverse
    ## use list to initialise compute all the internal functions
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse
         )
    
    
}

cacheSolve <- function(x) {
    ## get the cached inverse of the 'x' objects internal matrix ( 'xMatrix' )
    ## store as "cacheInverse"
    cacheInverse <- x$getInverse()
    
    ## if the inverse has been cached then return it and exit function
    if(!is.null(cacheInverse)) {
        message("returning cached matrix inverse")
        return(cacheInverse)
    }
    
    ## get the cached matrix inside the 'x' object and compute inverse with solve()
    cacheMatrix <- x$getMatrix()
    computeInverse <- solve(cacheMatrix)
    ## store the newly computed matrix inverse inside the 'x' object using setInverse
    x$setInverse(computeInverse)
    computeInverse

}
