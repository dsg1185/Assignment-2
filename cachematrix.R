##create a function that cache's the data

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL  #sets m to Null as the default 

    set <- function(y) {
        x <<- y    # Set the value for x
        m <<- NULL # sets the value for m regardless of the environment
    }

    get <- function() x #function to get the value of the inverse

    setinverse <- function(solve) m <<- solve # defines the function to get the value of the inverse

    getinverse <- function() m

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)


}


## returns the inverse of the function above

cacheSolve <- function(x, ...) {

    m <- x$getinverse() #gets the inverse of the function
    if(!is.null(m)) {
message("getting cached data")
        return(m)

    }

    data <- x$get()  
    m <- solve(data, ...) 
    x$setinverse(m) 
    m


        ## Returns matrix that is the inverse of x
}
