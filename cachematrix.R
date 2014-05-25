## The propose af the following prorgam is to make a cache for a time 
# consuming calculation. The program consist of two functions. 

# The first function will create special matrix and be able to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored value to NULL
    m <- NULL
    
    #1  set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #2 get the value of the matrix
    get <- function() x
    
    #3 set the value of the inverse matrix
    setsolve <- function(solve) m <<- solve
    
    #4 get the value of the inverse matrix      
    getsolve <- function() m
    
    # return the list
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# The second function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated and the 
# matrix has not changed, then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # check if the inverse is already cached
    m <- x$getsolve()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If not cached
    data <- x$get()
    # Calculate the inverse
    m <- solve(data, ...)
    x$setsolve(m)
    m # return the inverse
}
