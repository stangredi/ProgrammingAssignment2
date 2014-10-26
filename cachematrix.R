## Create 2 functions to store a special object that is a matrix and its 
## inverse. The inverse of the matrix after being calculated can be stored
## in the cache of the special object. When retrieving the inverse if there
## is a value in the cache then it will return that value as the inverse
## or it will recalculate the inverse and store in the cachec

## MakeCacheMatrix
## - will create the  custom matrix object
## - the new object will be returned as a matrix 
## - this new object will have the following functions
##  set() - inialize object and set inverse property to NULL
##  get() - returns the matrix object
##  setinverse() - stores the inverse in the inverse property of the object
##  getinverse() - returns the inverse from the stored inverse property
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
          x <<- y
          i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list( set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve
## this function assumes that the custom matrix object
## create with the MakeCacheMatrix function is used
##
## This function first checks to see if the the inverse
## of  the matrix is cached. If it is is will use that
## and return it from the function
##
##  if the inverse is not cached it will retrive the matrix from the object
##  Calculate the inverse using the solve function
##  then store the inverse in the cache of the object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
       message("getting cached data")
       return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
