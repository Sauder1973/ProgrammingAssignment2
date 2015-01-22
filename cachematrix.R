## Put comments here that give an overall description of what your
## functions do
## Written By Wes Sauder
## Write a short comment describing this function


# Function: "makeCacheMatrix" - creates a special "matrix" object that 
#  can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        ## 'x' is a function from the base package to create a matrix 
        ## with the following Parameters:
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        print(matrix())
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        print("Data")
        print(data)
        print("Answer m")
        m <- solve(data)
        print(m)
        mTWO <-solve(data)
        print("Answer mTWO")
        print(mTWO)
        #x$setmatrix(m)
        x$setmatrix(m)
        
        m
        print(m)
        
        
}



B = matrix(4:7,nrow=2,ncol=2)
print(B)

fList <- makeCacheMatrix(B)

Result <- cacheSolve(fList,B) #1st call, this will compute the inverse






## matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
##       dimnames = NULL)
## Arguments

## data:    
##        an optional data vector (including a list or expression vector). 
##        Non-atomic classed R objects are coerced by as.vector and all attributes discarded.

## nrow:
##      the desired number of rows.
## ncol:
##      the desired number of columns.
## byrow:
##      Logical. If FALSE (the default) the matrix is filled by columns, 
##      otherwise the matrix is filled by rows.
## dimnames:
##      A dimnames attribute for the matrix: NULL or a list of length 2 giving the row and column names 
##      respectively. An empty list is treated as NULL, and a list of length one as row names. 
##      The list can be named, and the list names will be used as names for the dimensions.
## x:        
##      an R object.
