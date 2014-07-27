
## makeCacheMatrix creates a cache to hold the inverse and data
## the cache stores the value in the environment of the makeCacheMatrix function for later retrieval
## the funtions get and set function as then returned as a list capturing the cached values from the outer function scope
makeCacheMatrix <- function(x = matrix()) {
		    i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
		    getinverse <- function() i
        l<-list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve either gets the cache's inverse if it already has it
## or else calculates the inverse, then caches returning the solution
cacheSolve <- function(x, ...) {
		    i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

#rough test code
#x=rbind(c(1, -1/4), c(-1/4, 1))  
#solve(x) # this result should equal this
#c<-makeCacheMatrix(x)
#cacheSolve(c)

