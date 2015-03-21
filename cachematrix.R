makeCacheMatrix <- function(x = matrix()) {
        
        invMatrix <- NULL       #Intitalized the inverse matrix variable 
                                #to NUll so the makeCacheMatrix function 
                                #doesn't hang first time through
        
        matrix1 <- x            #Set the matrix passed 'x' via the console into
                                #the function makeCacheMatrix to 'matrix1'
        
        getMatrix <- function() matrix1  #Create a function 'getMatrix' 
                                         #in makeCacheMatrix with matrix variable 'matrix1'
        
        setMatrix <- function(y = NULL) {   #Create a function 'setMatrix' that takes
                                            #takes in a matrix 'y' and sets it as a 
                                            #matrix variable 'matrix1' in the global 
                                            #environment hence <<-. i.e sets 'matrix1' to the cache.                                            
                matrix1 <<- y 
        }
        
        getMatrixInv <- function() invMatrix  #Create function 'getMatrixInv'
                                              #in  makeCacheMatrix that with matrix variable 'invMatrix'
                                              #(basically returns invMatrix variable,
                                              #but returns has a specific meaning)
        
        setMatrixInv <- function(solveM) invMatrix <<- solveM   #Create function 'setMatrixInv' in
                                                                #makeCacheMatrix that with matrix 
                                                                #variable 'invMatrix' that is the inv of
                                                                #'matrix1' and makes it availabe in the 
                                                                #global environment, hence <<-
                                                                #i.e sets the invMatrix to the cache
        
        
        list(getMat = getMatrix, setMat = setMatrix, getMatInv = getMatrixInv, setMatInv = setMatrixInv)
        #creates a list of four functions;        
}


cacheSolve <- function(x, ...) {
                                                 #Return a matrix that is the inverse of 'x'. 
                                                 #the list from makeCacheMatrix function is passed into it.
        solveM <- x$getMatInv()                  #makes variable solveM the function 'getMatInv()' from MakeCacheMatrix list 
        
        if(!is.null(solveM)){                    #Conditional statement checking to see if the matrix for inversion  
                message("getting cached data")   #is in the cache. If it is then it pulls it from cache and returns it.
                return(solveM)                   #this is why invMatrix in makeCacheMatrix function must be int. to NULL
        }
        
        theMatrix <- x$getMat()                  #Assign a variable called "theMatrix" to be the matrix for inversion
        solveM <- solve(theMatrix, ...)          #Invert the matrix
        x$setMatInv(solveM)                      #Call the function setMatInv from the list which sets the inverted matrix to the cache
        
}