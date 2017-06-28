## makeCacheMatrix and cacheSolve facilitate storage of a matrix and it's inverse so
##    that one can search for a cached inverse rather than computing a new one if an 
##    inverse has already been applied


## makeCacheMatrix returns a list of functions (set, get, setsolve, getsolve) which 
##     serve as scoped storage for a matrix and its inverse
## The set function will store a matrix within the function scope and set the 
##     matrix inverse to NULL
## The get function will return the matrix stored within the scope of this funtion
## The setsolve function will store a value to an inverse matrix placeholder 
##     within the function scope
## The getsolve function will return the value stored in the inverse matrix 
##     placeholder stored within the scope of this function or a null if no inverse 
##     has been populated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## CacheSolve is desgined to work in conjunction with a makeCacheMatrix object
## When a makeCacheMatrix object is called within cacheSolve, cachesolve will check to see
##     if a inverse value is stored within the object.
## If so, a message "getting cached data" is returned along with the stored value.
## If not, an inverse is computed and stored in within the makeCacheMatrix inverse placeholder.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
