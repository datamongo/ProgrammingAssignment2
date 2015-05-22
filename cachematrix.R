## makeCacheMatrix is a function that returns a vector which is a list
## of functions. The list elements
## get - gets the value of the vector
## set - sets the value of the vector
## setinv - finds the value of the inverse of the matrix
## getinv - gets the value of the inverse of the matrix
## 
## cacheSolve solves the matrix and finds its inverse. If it is a new matrix,
## the inverse is calculated, otherwise if the inverse of the matrix entered
## has been calculated previosuly, the cached value of the matrix inverse 
## is output

## The functions set, get, setiv and getinv are defined inside of makeCacheMatrix
## The set and setinv functions set the return values to be global values
## using the <<- operator. m is also initialized as a global variable. The
## setinv function returns the inverse of the matrix and stores it as a
## global variable m which can then be retrieved using getinv

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## CacheSolve returns the inverse of a a matrix that is passed as an
## argument to it. If the variable m is NULL then no matrix has been cached,
## a new matrix is read into data and its inverse calculated, stored in
## in variable m and returned. If the matrix inverse has already been
## calculated then it is stored in m and the if loop is executed and the
# cached value of m is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
