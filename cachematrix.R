# Assignment2 : Caching the Inverse of a Matrix

## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        ##Create a special matrix object that can cache its inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

# Test
matd<-matrix(c(1,2,3,4),nrow=2,ncol=2)
x <- makeCacheMatrix(matd)
x$get()
cacheSolve(x)     # This will calculate inverse
cacheSolve(x)    # This time we get cached inverse
getting cached data