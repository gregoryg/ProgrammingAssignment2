## Create a matrix and lexically scoped function 
## Follow the code in the example for this assignment
## ???
## Profit!
## Pass in matrix, return list with setters and getters
## test on example from forum: matrix(c(1,1,1,3,4,3,3,3,4),3,3)  
## more tests at https://class.coursera.org/rprog-009/forum/thread?thread_id=164
makeCacheMatrix <- function(x = matrix()) {
    invertedMatrix <- NULL
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invertedMatrix <<- inverse
    getinverse <- function() invertedMatrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## list with function closures!
}

## Use the R solve() function to invert a matrix - return prior results if cached
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## NO CHECKING for invertable matrix
    invertedMatrix <- x$getinverse()
    if (!is.null(invertedMatrix)) {  ## if not null, we found a cache hit
        message("getting cached matrix")
        return(invertedMatrix)
    } else {  ## cache miss; else clause clearer IMO
        invertedMatrix <- solve(x$get()) ## execute the function, don't return it
        x$setinverse(invertedMatrix) ## now set the cache so we don't recalculate on another invocation
        return(invertedMatrix)
    }
}

