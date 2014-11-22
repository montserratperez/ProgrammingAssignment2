## The functions below are used to create a special object that stores
## a matrix and caches its inverse

## We create four functions within makeCacheMatrix and it returns a 
## list with the values of these four functions

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <-function ()x
        setsolve <- function (Solve)m <<-solve
        getsolve <-function()m
        list ( set=set,get=get,
               setsolve=setsolve,
               getsolve=getsolve)
}


## CacheSolve calculates the inverse of the matrix but checks first to 
## see it it has already been calculated, in which case it skips the 
## computation. Otherwise, it calculates the inverse and sets the 
## values in the cache via the setsolve function

cacheSolve <- function(x, ...) {
        m <-x$getsolve()
                if (!is.null(m)){
                        message ("getting cached data")
                        return (m)
                }
        data <-x$get()
        m <-solve (data, ...)
        x$setsolve (m)
        m
        ## Return a matrix that is the inverse of 'x'
}
