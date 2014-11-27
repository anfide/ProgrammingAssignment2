## Maintain a "matrix vector" that holds a matrix and its inverse.
# ( NOTE: It could be built by a simple vector with two elements (matrix, inverse)
# but I think there would be one main disadvantage:
# the update of the inverse value would be complete responsibility of the 
# matrix user
# 



## Create a special "matrix vector" object to hold a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse = NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cached_inverse <<- inverse
    getinverse <- function() cached_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## returns the inverse of a "matrix vector" created by a call to makeCacheMatrix.
# The inverse is computed on the first call after the matrix in x changes.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    m
}
