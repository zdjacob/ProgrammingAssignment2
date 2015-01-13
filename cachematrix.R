##        STEP BY STEP PROGRESS OF THE CODE

## makeCacheMatrix is the function that records the input matrix and has the
## get,set functions that are required to input/access the input matrix 
## and access the Inverse that is calculated. This is where the inverse that is calculated
## is cached

makeCacheMatrix <- function(x = matrix()) {

    inverseRecorded<-NULL
    
    setInverse<-function(inverseInput) {inverseRecorded<<-inverseInput}
    
    get<-function() x
    
    getInverse<-function() inverseRecorded
    
    list(get=get,getInverse=getInverse,setInverse=setInverse)
}

##        STEP BY STEP PROGRESS OF THE CODE
## The cacheSolve takes the list that is returned by makeCacheMatrix as input.
## The function gets the registered inverse that is present using getInverse()
## Since we initialize the inverseRecorded with NULL, I am using the is.null function

## If the inverse that is registerd is null then the inverse from the memory is returned.
## without doing any calculation of the inverse explicitly.

## Else the inputted matrix is retrieved using the "get" function
## The inverse of the matrix is found using the "solve" function
## We set the inverse value then using the "setInverse" function
## and then return the currently calculated inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$getInverse()
  
  if(!is.null(inverse)){
    message("Inverse is cached. Getting from there...")
    return(inverse)
  }
  else message("Inverse not cached. Calcing the inverse...")
  
  matrixInputted<-x$get()
  
  inverse<-solve(matrixInputted)
  
  x$setInverse(inverse)
  
  return(inverse)
}

