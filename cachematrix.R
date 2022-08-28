## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inve <- NULL                             ## initialize inve as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function for assign new 
        x <<- y                             ## value of matrix in parent environment
        inve <<- NULL                        ## if new matrix, reset inve to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument

    setinverse <- function(inverse) inve <<- inverse  ## assigns value of inve
    getinverse <- function() inve                    ## gets the value of inve 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}



## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),then cacheSolve will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inve <- x$getinverse()
    if(!is.null(inve)) {
        message("getting cached data")
        return(inve)
    }
    data <- x$get()
    inve <- solve(data, ...)
    x$setinverse(inve)
    inve
}

