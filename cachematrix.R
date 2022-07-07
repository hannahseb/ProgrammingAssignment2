## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      invers <- NULL    # initialized, will be used later
      set <- function(y) {
            x <<- y     # assignment input argument x obj in parent enviro
            invers <<- NULL # assign NULL to invers obj in parent enviro
                              # clears any val of invers cached before in cacheSolve
      }
      
      get <- function() x
      setinverse <- function(inverse) invers <<- inverse # need to be accessed after setmean(), <<- for global enviro
      getinverse <- function() invers # for retrieving directly without further computation
      list(set = set, get = get, #necessary to access via $
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
# necessary to compute inverse of matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invers <- x$getinvers()
      if(!is.null(invers)) { #check if already invers in cache
            message("getting cached data")
            return(invers)
      }
      
      # if no cached data,..
      data <- x$get() #..get matrix from input obj
      invers <- solve(data, ...) #calculate inverse
      x$setinverse(invers) # use setinvers to set inverse in input object
      invers
}


my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
my_matrix$getinverse()
my_matrix$set(matrix(c(2,2,1,4),2,2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
