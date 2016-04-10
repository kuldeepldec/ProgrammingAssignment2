#makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {   #set the value of the matrix to null
                x <<- y
                m <<- NULL
        }
        get <- function() x    #get the value of the matrix
        setinverse <- function(inverse) #set the value of the matrix
        m <<- inverse
        getinverse <- function() m    #get the value of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



#Cache solve computes the inverse of the special "matrix" returned by makeCacheMatrix 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data") #matrix not change it retrieve data from cache
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)   #calculates inverse of the matrix
        x$setinverse(m)
        m
}
