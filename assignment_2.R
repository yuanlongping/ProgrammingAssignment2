##The following function is used to create a special object that stores a matrix and 
##its inverse.

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {                                     #set a matrix
                    x <<- y
                    m <<- NULL
              }
              get <- function() x                                      #get a matrix   
              setinverse <- function(inverse)  m <<- inverse           #set its inverse
              getinverse <- function() m                               #get its inverse
              list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##The following function calculates the inverse of the special "matrix" created with the 
##above function. However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
##calculates the inverse of the matrix and sets the value of the data in the cache via the 
##setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting solved data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
