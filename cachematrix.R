
## makeCacheMatrix creates and returns functions 
## used to get the inverse of a square matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        ## initialize m
        m <- NULL
        
        ## print environment
        print(environment())
        
        ## set evn to environment
        evn <- environment()
        
        ## print parent environment of evn
        print(parent.env(evn))
        
        ## set the matrix in the working environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## returns the value of matrix x
        get <- function() x
        
        ## set m to what is passed in
        setinv <- function(inv) m <<- inv
        
        ## return the value of m
        getinv <- function() m
        
        ## set getevn to the current environment
        getevn<- function() environment()
        
        ## return the values from the $set, $get, $setmean, $getmean, $getevn
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv,
             getevn = getevn)

}


## calculates the inverse of a square matrix
## if the inverted matrix does not exist in cache,
## it is created and stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check to see if the inverse is already cached
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                ## return the cached matrix
                return(m)
        }
        
        ## since the matrix does not exist, create it
        ## first, get the matrix to invert
        data <- x$get()
        
        ## calculate the inverse of the matrix
        m <- solve(data, ...)
        
        ## chache the inverse
        x$setinv(m)
        
        ## return the inverse
        return(m)
}
