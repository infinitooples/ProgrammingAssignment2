##These functions use the built in solve() function to calculate the inverse of a matrix, check if the inverse of that matrix
##was already solved, and then return a cached value of the inverse if the matrix was in fact already solved.  The functions
##should be used together so that if M is the matrix inverse.M <- cacheSolve(makeCacheMatrix(M))

##makeCacheMatrix takes a matrix as an input and creates four utility functions as an output.  
##These functions are used by cacheSolve.  set() creates a value for the matrix, get() returns the value for the matrix,
##setinv() creates a value for the inverse of the matrix, and getinv() returns the value of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
