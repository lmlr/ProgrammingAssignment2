## The functions makeCachceMatrix and cacheSolve are used to
## create a special object that stores a matrix and chaches its inverse.

## makeCachesMatrix takes a matrix 'x' as an input and creates
## a list of functions to set and get the value of the matrix (set, get) and
## the value of the inverse of the matrix (setinverse, getinverse).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ##function to set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x ##get the stored matrix
    setinverse <- function(inv) m <<- inv ##set the inverse of the matrix
    getinverse <- function() m ##get the stored inverse of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if the inverse has already been calculated.
## If the inverse has not been calculated and stored, the function
## calculates the inverse and stores its value via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() ##get the current value inverse of the matrix stored in x.
    if(!is.null(m)){ ##if m is not NULL, the inverse is already stored in x.
        message("getting cached data")
        return(m) ##return the stored inverse
    }
    data <- x$get()
    m <- solve(data) ##calculate the inverse
    x$setinverse(m)  ##store the calculated inverse
    m                ##return the calculated inverse
}
