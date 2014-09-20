makeCacheMatrix <- function(x = matrix()) {
    ## function with variable 'x' which is the matrix to be operated on
    ## returns a list of functions containing:
        ## set - sets value of matrix
        ## get - retrieves value of matrix
        ## setinverse - sets inverse of matrix which can be generated from cacheSolve (see below)
        ## getinverse - retrieves inverse of matrix
    s <- NULL
    ##creates value of s
    set <- function(y) {
        x <<- y
        s <<- NULL
    ## function 'set', saves input with x called from function environment
    }
    get <- function() x
    ## function 'get', retrieves x
    setinverse <- function(solve) s <<- solve
    ## function 'setinverse' sets s as solve called from function environment
    getinverse <- function() s
    ## function get inverse, retrieves value of s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    ## Variable x correpsonds to output generated from makeCacheMatrix (see above)
    ## outputs inverse of matrix by using the solve function by:
    ## either returning a previously calculated inverted matrix
    ## or calculuting a new inverted matrix
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
        ## terminates if s is not null, and prints makes s as output
    }
    data <- x$get()
        ##retrieves matrix
    s <- solve(data, ...)
        ##creates inverse of matrix and saves as variable 's'
    x$setinverse(s)
        ##sets inverse
    s
}