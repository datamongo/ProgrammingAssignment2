## makeCacheMatrix is a function that returns a vector which is a list
## of functions. The list elements are
## get - gets the value of the vector
## set - sets the value of the vector
## setinv - finds the value of the inverse of the matrix
## getinv - gets the value of the inverse of the matrix
## 
## usage: a <- makeCacheMatrix(matrix(data,nrow,ncol,...))
##        cacheSolve(a)
## 
## cacheSolve solves the matrix and finds its inverse. If it is a new matrix,
## the inverse is calculated, otherwise if the inverse of the matrix entered
## has been calculated previosuly, the cached value of the matrix inverse 
## is output. The inverse is calculated using the solve function in R

## The functions set, get, setiv and getinv are defined inside of makeCacheMatrix
## The set and setinv functions set the return values to be global values
## using the <<- operator. matinv is also initialized as a global variable.
## The setinv function returns the inverse of the matrix and stores it as a
## global variable matinv which can then be retrieved using getinv

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL

        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinv <- function(solve) {
                matinv <<- solve
        }
        
        getinv <- function() {
                matinv
        }
        
## Return a list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## CacheSolve returns the inverse of a matrix that is passed as an
## argument to it. If the variable matinv is NULL then no matrix has been
## cached, a new matrix is read into data and its inverse calculated, stored
## in variable matinv and returned. If the matrix inverse has already been
## calculated then it has been stored in matinv, the if loop is executed and
## cached value of matinv is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinv()
        
        if(!is.null(matinv)) {
                message("Getting cached Matrix ... ")
                return(matinv)
        }
        
        data <- x$get()
        
        matinv <- solve(data, ...)
        
        x$setinv(matinv)
        
        matinv
}
