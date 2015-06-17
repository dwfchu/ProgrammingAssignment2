##caching functions for inverse matrix calculation

## define list of functions for get/set caching

makeCacheMatrix <- function(x = matrix()) {
       invmtx <- NULL
       set <- function(y) {
              x <<- y
              invmtx <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) invmtx <<- inverse
       getinverse <- function() invmtx
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## setting/getting cache


cacheSolve <- function(x, ...) {
       invmtx <- x$getinverse()
       if(!is.null(invmtx)) {
              message("getting cached data")
              return(invmtx)
       }
       data <- x$get()
       invmtx <- solve(data, ...)
       x$setinverse(invmtx)
       invmtx
}

inverseCalc <- function(matr) {
       
       tempMatr = makeCacheMatrix(matr)
       
       start.time = Sys.time()
       cacheSolve(tempMatr)
       dur = Sys.time() - start.time
       print(dur)
       
       start.time = Sys.time()
       cacheSolve(tempMatr)
       dur = Sys.time() - start.time
       #print(cacheSolve(tempMatr))
       print(dur)
       
}