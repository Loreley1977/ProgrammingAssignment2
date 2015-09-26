## here i created a function starting with a null matrix argument
makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL ## i is the matrix inverse, make the value NULL
        set <- function(y) { ## create another function where the value will be in
                x <<- y ## change the of the inversed matrix in case the matrix was changed
                i <<- NULL
        }
        get <- function() x ## gets the value of the inversed matrix
        setInverse <- function(inverse) i <<- inverse ## use the inverse function to calculate the inverse of the matrix
        getInverse <- function() i ## passes the value of the function makeCacheMatrix
        list (set = set, 
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}

## create function to get the cache of the matric
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) { ## if the inverse exists it gets it
                message("getting cached inverse of matrix")
                return(i)
        }
        matrix <- x$get() ## if the inverse is not there, it is calculated and then it fetches it
        i <- solve(matrix,...)
        x$setInverse(i)
        i
}
