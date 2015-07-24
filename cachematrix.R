## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  The makeCacheMatrix function creates list of functions to:
##    set - store the matrix data
##    get - get cached matrix data
##    setinv - stores the inverse of a matrix data
##    getinv - get the cached inverse of a matrix data
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set - store the matrix data
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get - get cached matrix data
        get <- function() x
        ## setinv - stores the inverse of a matrix data
        setinv <- function(solve) m <<- solve
        ## getinv - get the cached inverse of a matrix data
        getinv <- function() m
        ## List of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## Write a short comment describing this function
##  The cacheSolve returns the inverse of a matrix.
##  If the inverse of the matrix previously cached throught makeCacheMatrix
##   already has the inverse matrix data alredy in cache the it returns
##   the cached data. Otherwise, the function retrieves the matrix cached
##   data, calculates the inverse of the matrix, cache and returns the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
