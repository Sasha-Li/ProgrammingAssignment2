## Below are two functions that create a special object 
## that stores a matrix and cache's its inverse matrix

## The first function, makeCacheMatrix, creates list of functions that
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    s <- NULL                     # set default value of s to NULL, in case cacheSolve hasn't been used
    y <- NULL                     # set default value of y to NULL, in case cacheSolve hasn't been used
    setmatrix <- function(y) {    # set the value of the matrix
            x <<- y               # cache inputted matrix in order for cacheSolve to check whether it has changed
            s <<- NULL            # set s to NULL (the inverse if cacheSolve was used)
        }
        getmatrix  <- function() x                # get velue of matrix
        setinverse <- function(solve) s <<- solve # set inverse matrix
        getinverse <- function() s                # get the inverse matrix
        list(setmatrix = setmatrix, getmatrix = getmatrix, # create a list to keep the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, calculates the inverse of the matrix calculated above, 
## by checking first if the inverse has already been calculated and whether the matrix has changed. 
# If the matrix hasn't changed and the inverse has been calculated, it gets the inverse from the 
# cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the inverse 
# of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    s <- x$getinverse()                      # get inverse if it has been calculated
    if(!is.null(s)) {                        # check if cacheSolve has been run before
        #if(x$setmatrix() == x$getmatrix()) { # check if matrix has changed
        message("getting cached data")       # if not, send a text message and return the cached matrix
        return(s)                            # and return the cached matrix
        #}    
    }
    data <- x$getmatrix()                    # get the value of the input matrix 
    x$setmatrix(s)                           # cache input matrix
    s    <- solve(data, ...)                 # compute inverse matrix
    x$setinverse(s)                          # cache the inverse matrix
    s                                        # return inverse matrix
}

