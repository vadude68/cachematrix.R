##These two functions allow the user to compute the inverse of a square matrix
##and cache the value and inverse of that particular matrix.  Thus, if the
##inverse of this same matrix is needed again, it will not have to be recomputed.

## makeCacheMatrix creates a "special" matrix, which is actually a list of four
## functions. The "get" and "set" functions get and set the value of the matrix,
## while the "getinverse" and "setinverse" do the same for the computed inverse 
## of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix ceated by the previous
## function.  It first checks to see if the inverse has been calculated with 
## the getinverse function.  If so, it retrieves that value from the cache and 
## returns it.  If not, it calculates the inverse and then saves that value
## in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
