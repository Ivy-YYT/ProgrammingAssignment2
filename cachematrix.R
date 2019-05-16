## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix creates a function that store the matrix inversion. 
#cacheSolve is used to check whether a matrix inversion is the stored or not. 
#If not, the new matrix inversion will be stored.


## Write a short comment describing this function
#makeCacheMatrix is a function that contains four functions inside. When a matrix is applied, the printed results is a list of the four functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinversion <- function(solve) inv <<- solve
        getinversion <- function() inv
        list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


## Write a short comment describing this function

# Apply the results of above printed list here, and the cacheSolve function will first check if the matrix is stored. If yes, the codes from "data <- x$get()"
# will not be executed; if no, the matrix will be inversed.


cacheSolve <- function(x, ...) {
        inv <- x$getinversion()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinversion(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
        
        # check functions:
        # > t <- makeCacheMatrix(matrix(c(2, 4, 7, 12, 23, 45, 2, 9, 12), 3, 3))
        # > cacheSolve(t)
        # [,1]  [,2]  [,3]
        # [1,]  3.225  1.35 -1.55
        # [2,] -0.375 -0.25  0.25
        # [3,] -0.475  0.15  0.05
        
}
