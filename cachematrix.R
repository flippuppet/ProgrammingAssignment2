## Put comments here that give an overall description of what your
## functions do

## this function creates a cache to hold the inverse and data

makeCacheMatrix <- function(x = matrix()) {
		    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
		    getinverse <- function() m
        l<-list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function either gets the caches inverse or calculates the inverse caches it then returns it

cacheSolve <- function(x, ...) {
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


#x=rbind(c(1, -1/4), c(-1/4, 1))  
#solve(x)
#c<-makeCacheMatrix(x)
#cacheSolve(c)