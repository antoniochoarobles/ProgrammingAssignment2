## Put comments here that give an overall description of what your
## functions do

## The first function, makeVector creates a special "vector", which is really a list containing a function to
##    set the value of the vector
##    get the value of the vector
##    set the value of the mean
##    get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

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
