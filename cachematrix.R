## R functions that are able to cache potentially time-consuming computations
## which involve caching the Inverse of a Matrix
## Written By Wes Sauder
## January 20, 2015



# Function: "makeCacheMatrix" - creates a special "matrix" object that 
#       can cache its inverse for quick retrieval using cacheSolve below.
makeCacheMatrix <- function(x = matrix()) {

        ## 'x' is a function from the base package to create a matrix 
        ## with the following Parameters:
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}

## This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above unless the inverse has been
##      calculated previously - Function retrieves inverse from cache
##      if calculation has been performed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        
        ##Check if inverse calculation exists - return inverse of matrix
        ##      stored in cache and exit function
        if(!is.null(m)) {
                message("getting cached data")
                ##print(m)    ## uncomment to validate returned matrix (Previously Calc IN Cache)
                return(m)
        }
        data <- x$get()
        
        m <- solve(data)
        
        mTWO <-solve(data)
        
        x$setmatrix(m)
        ##print(m)    ## uncomment to validate returned matrix (NOT in Cache)
        
        m
        
        
}


## Test Code for Example:
## remove comments to try code
## Confirmation Dataset:

        #a <- matrix(c(2,3,5,7,11,13,17,19,23),3,3)
        #b <- solve(a)

## Results for b should be as follows:
## [,1]       [,2]        [,3]
## [1,] -0.07692308 -0.7692308  0.69230769
## [2,] -0.33333333  0.5000000 -0.16666667
## [3,]  0.20512821 -0.1153846 -0.01282051

#       fList <- makeCacheMatrix(a)
#       Result <- cacheSolve(fList,a) #1st call, this will compute the inverse

## Try again and see if answer will return from cach


#       c = a
#       fList_c <- makeCacheMatrix(c)
#       Result_c <- cacheSolve(fList,c) #1st call, this will compute the inverse

## The following should return value from the cache
##getting cached data
##[,1]       [,2]        [,3]
##[1,] -0.07692308 -0.7692308  0.69230769
##[2,] -0.33333333  0.5000000 -0.16666667
##[3,]  0.20512821 -0.1153846 -0.01282051

