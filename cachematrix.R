# makeCacheMatrix function
# This function is to create a special matrix that can 
# cache its inverse

makeCacheMatrix <- function(m = matrix()){
  n <- NULL
  set <- function(x){
    m <<- x
    n <<- NULL
  }
  get <- function() m
  setinv <- function(inv) n<<-inv
  getinv <- function() n
  list(set=set, get=get, setinv=setinv, getinv=getinv)	
}

# cacheSolve function
# The function is to find the inverse of an invertible matrix. 
# If the inverse has been calculated with the same matrix, 
# it will not calculate again instead giving out the cached data. 
# If a new matrix is entered, a new inverse will be calculated.

cacheSolve <- function(m=matrix()){
  n <- m$getinv()
  if(!is.null(n)){
    message("getting cached data")
    return(n)
  }
  data<- m$get()
  n<-solve(data)
  m$setinv(n)
  n
} 