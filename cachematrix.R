#Inversion of a matrix over a large dataset may be a cost operation in terms of 
#execution time and performance. Caching can be used to retrieve the results 
#rather than computing the result again. We can use following function to cache 
#the inverse of a matrix
 
#makeCacheMatrix function has functions to set the value of matrix, get the 
#value which is set, set the inverse of the matrix and get the inverse. This 
#function creates a list containing above function inside it

makeCacheMatrix <- function(x = matrix()) {

   inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


#This function returns the inverse of the matrix. It first checks if
#the inverse has already been computed. If so, it gets the result and skips the
#computation. If not, it computes the inverse, sets the value in the cache via
#setinverse function.

#Note: This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {

    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


