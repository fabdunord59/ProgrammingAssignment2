# The function creates some functions used to inverse a matrix:
# setinverse, getinverse, get, set
makeCacheMatrix <- function (m = matrix()){
        # matrix in cache
        cachee <- NULL
        # create the matrix
        set <- function(y) {
                m <<- y
                cachee <<- NULL    
        }
        # get the matrix
        get <- function() m
        # compute inverse and store in cache
        setinverse <- function(inverse) cachee <<- inverse
        # get from cache
        getinverse <- function () cachee
        # print the functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Compute the inverse of the matrix m defined by makeCacheMatrix
# if the matrix isn't in cache then the inverted is calculated and stored in cache
cacheSolve <- function(m, ...) {
        inverse <- m$getinverse()
        # return the matrix if exists in cache
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # create the matrix if not in cache 
        data <- m$get()
        # compute the inverse
        inverse <- solve(data, ...)
        # save in cache
        m$setinverse(inverse)
        # return in console
        inverse
}
