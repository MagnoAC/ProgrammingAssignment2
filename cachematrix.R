## makeCacheMatrix is function used to create an Cache 
## Matrix. It is impotant to say that we must consider that the 
## matrix inputed will always be invertible

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { # set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x # get the value of the matrix
        setinverse <- function(solve) m <<- solve #set the inverse of the matrix
        getinverse <- function() m # get the inverse of the matrix
        list(set = set, get = get, # return the list used in cacheSolve
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will use the list returned on makeCacheMatrix to, firstly, see
## if the inverse calculation was already done. If it is not, it will make the
## the inverse calculation and return it value. Otherwise, it will return the
## previously made calculation.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() #checks if the inverse was already calculated
        if(!is.null(m)) { #in case of already being calculated.
                message("getting cached data")
                return(m)
        }
        data <- x$get() #get the value of the matrix
        m <- solve(data, ...) #invert the matrix
        x$setinverse(m) #will fix the value of the setted matrix
        m ## Return a matrix that is the inverse of 'x'
}
