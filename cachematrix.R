## Put comments here that give an overall description of what your
## functions do

## Function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL ##initializing inverse property
        set<- function(matrix){
                ##setting the matrix
                x<<-matrix 
                i<<-NULL
        }
        ## retrieving the matrix
        get<-function(){
                x
        }
        ##setting the inverse
        setInverse<-function(inverse){
                i<<-inverse
        }
##getting the inverse of matrix
        getInverse<-function(){
                i
        }
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## computes inverse of special matrix, if not already, using makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        ##
        if(!is.null(m)){
                ##checking cache and returning if true
                message("Getting Data")
                return(m)
        }
        ##getting matrix from object
        d<-x$get()
        ##calculating inverse
        m<-solve(d)
        ##setting inverse
        x$setInverse(m)
        ##returning matrix
        m
}
