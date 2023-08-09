## This file is for Coursera R Programming Week 3 Programming Assignment 2: Lexical Scoping
## The function aims to caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){ 
        x <<- y
        inv <<- NULL
    }
    get <- function()x      
    setinverse <- function(inverse)inv <<- inverse        
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}

# The following is an example to test if the function works
my_Matrix <- makeCacheMatrix()
my_Matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
result <- cacheSolve(my_Matrix)
print(result)
