##creates a special "vector", which is a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverseMat <- function(invMat) i <<- invMat
        getInverseMat <- function() i
        list(set = set, get = get,
             setInverseMat = setInverseMat,
             getInverseMat = getInverseMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

        i <- x$getInverseMat()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverseMat(i)
        i

        ## Return a matrix that is the inverse of 'x'
}
