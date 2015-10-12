##Lyle A.Stewart
#R Programming Cache Matrix 

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

    # makeCacheMatrix creates a list containing a function to
        # 1. set the value of the matrix
        # 2. get the value of the matrix
        # 3. set the value of inverse of the matrix
        # 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #create object inv and set it to NULL. means it is undefined and will be returned by expression
    set <- function(y){
        x <<- y
        inv <<- NULL # remember <<- is used to assign value to an object in a different environment
        ## so we are maintaining that inv is set to be NULL in all environments; in both colsure and encolsure funciton(s). 
        
    }
    get <- function()x # create variable named get and set it to be a fun *x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse  <- function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#just created a function named makeCacheMatrix with one argument, x, and x is a matrix 

# So now that we have assigned value to the matrix,
# let us with the function below find inverse of 
# the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) { #create the function cacheSolve with 2 arguments, x and the ... operator 
    inv <- x$getinverse() # note that ... is used to match unspecified formal arguments of the function cacheSolve. We in essence use .. .to pass arguments through even through they are not formal  
    if(!is.null(inv)) { # we want the data that is not NULL
        message("getting cached data.") #this creates a diagnostic message while we wait for the program to execute and pull the data
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
