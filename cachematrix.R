## the functions hereafter presented aim to return the inverse of a matrix (presumed squared and invertible)
##and since matrix inversion is usually time-consuming, it is advisable to cache the inverse of a matrix.
##the 2 functions here below, altogether cache the inverse of a matrix.


## the function makeCacheMatrix creates  a special "matrix" object that can cache 
#its inverse. To calculate the inverse, function "solve" is used.

makeCacheMatrix <- function(x = matrix()) {
        #x is a square invertible matrix
        #makecachematrix is a function that returns a list with functions that:
        #set the matrix and get the matrix
        #set the inverse and get the inverse
        
        inv.matr<- NULL
        setmatrix <- function(y){
                x<<- y
                inv.matr<<- NULL
        }
        getmatrix <- function ()x
        
        setinverse <-function(solve) inv.matr<<- solve 
        getinverse<- function() inv.matr
        list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
       
}


## This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cacheSolve should retrieve the inverse 
#from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.matr<- x$getinverse()
        if(!is.null(inv.matr)) {
                message ("getting cache data")
                return(inv.matr)
        }
        dat<-x$getmatrix()
        inv.matr<-solve(dat, ...)
        x$setinverse(inv.matr)
        inv.matr
}

#test1: inverse matrix
prova<-matrix(c(1,2,2,3), nrow=2, ncol=2)
prova

test<-makeCacheMatrix(prova)
cacheSolve(test)
