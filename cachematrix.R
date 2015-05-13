## This initializes a matrix-like object: actually a list whose
## elements are object "methods." The variables in the function close
## over the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(m) {
        x <<- m
        inv <<- NULL
    }

    get <- function() x

    setinv <- function(inverse) inv <<- inverse

    getinv <- function() inv

    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Uses a matrix-like object from makeCacheMatrix. Checks to see if
## inv is cached. If it is, it uses it. If not, it figures it out, and
## then caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data.")
        return(inv)
    }
    my.matrix <- x$get()
    inv <- solve(my.matrix)
    x$setinv(inv)
    inv
}
