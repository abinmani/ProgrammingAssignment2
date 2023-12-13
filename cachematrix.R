## The function makeCacheMatrix takes in a empty matrix object
##and gives a list of functions set, get setinv and get inv.
## when each of the functions are invoked set gives value to the empty matrix,
##get GETS the matrix when needed and setinv and getinv does the same for the inverse.


makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        set<- function(y){
                x<<- y
                inverse<<- NULL
        }
                get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
                
                
        }
}


## this function calculates the inverse if its not already calculated once 
##and stores the value in x object of makeCacheMatrix.
## solve() is used to invert the matrix


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
