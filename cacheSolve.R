#Cache solve computes the inverse of the special "matrix" returned by makeCacheMatrix 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)   #calculates inverse of the matrix
        x$setinverse(m)
        m
}
