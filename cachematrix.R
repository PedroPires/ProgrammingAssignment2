## As requested, this function will create a special matrix that can cache
## its inverse, saving some time when inverting a matrix using R

## The makeCacheMatrix will create a specia matrix (list) that will actually
## perform the actions in the list:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse
## 4. get the values of the inverse
## This function is really similar to the example provided in the assigment
## description because is the same logic, just a different object and
## a different function. The only required modifications were the definition
## of x and the function "solve" (and some terms, of course).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix set using "makeCacheMatrix",
## but will retrieve the inverse from cache if it has already been calculated. 

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
        ## Return a matrix that is the inverse of 'x'
## Both functions were tested with different matrices and worked as expected.
