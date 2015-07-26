###################################################################
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix caches a matrix and for use in 
## for matrix inversion. The caching will check
## has already been inverted previouly
##
## Example use: 2x2 matrix
##
## A = [1 2 
##      3 4]
## A <- matrix(c(1,2,3,4),nrow=2,ncol=2)
##
## Create Cache of  matrix
## A_Cache <-  makeCacheMatrix(A)
#
# Invert the matrisx
#
# A_Inv <- cacheSolve(cache_A)
#
# A_Inv 
# A_Inv
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
# Invert the same matrix again
#
# A_Inv <- cacheSolve(cache_A)
#
# getting cached data
#
###################################################################

makeCacheMatrix <- function(x = matrix()) {
+        m <- NULL
+        set <- function(y) {
+                x <<- y
+                m <<- NULL
+        }
+        get <- function() x
+        setinv <- function(sol) m <<- sol
+        getinv <- function() m
+        list(set = set, get = get,
+             setinv = setinv,
+             getinv = getinv)
}


## Write a short comment describing this function
## Th
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
+        m <- x$getinv()
+        if(!is.null(m)) {
+                message("getting cached data")
+                return(m)
+        }
+        data <- x$get()
+        if(nrow(data) == ncol(data)){
+                m <- solve(data, ...)
+                x$setinv(m)
+            
+        }else{
+                message("Not a Square Matrix")
+        }
+        m   
}
