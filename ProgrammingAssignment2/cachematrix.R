## The makeCacheMatrix function serves creates a matrix and a place to cache the it and its inverse for later usage.
## This potentially reduces computing costs in the future.  The function cacheSolve first looks to see if an inverse
## of the matrix has been stored.  If not it will solve for the inverse and cache it for next time. User will be notified
## if cached matrix is used.

## This function creates a matrix and creates a place to cache the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #create empty object for m
    i <- NULL
    #set the values of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #get the values of the matrix
    get <- function() x
    #set the inverse of the matrix
    setinv <- function(solve) i <<- solve
    #get the inverse of the matrix
    getinv <- function() i
    #returns a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#Test results
x = matrix(1:4, nrow = 2, ncol = 2)
m = makeCacheMatrix(x)
m$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

## This function first looks for a cached version of the inverse of the matrix. If one does
## not exist it computes the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    #checks if there is something cached
    #if there is returns message and cached matrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    #if there is nothing cached gets matrix
    data <- x$get()
    #solves for inverse
    i <- solve(data, ...)
    #sets it
    x$setinv(i)
    #returns inverted matrix
    i
}

# Test results
cacheSolve(m)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
cacheSolve(m)
#getting cached data
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5