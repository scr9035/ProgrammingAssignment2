makeCacheMatrix <- function(x = matrix()) {
                                                    #Essentially creates a list of four functions. Checks to see
                                                    #if the matrix is in cache, if so move on, if not set it to cache.
                                                    #Also checks to see if the inverse of a matrix asked to be inverted
                                                    #is in the cache. If so, pass that, if not invert the matrix the set it to cache.
                                                    #The control of these functions is more clearly seen in the function
                                                    #that calls the list. The cacheSolve function below.
        
        
        
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
                                              #but return has a specific meaning so I don't want to use that word.)
        
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
                                                 #Return a matrix that is the inverse of 'x'. If the matrix inversion
                                                 #in the cache it doesn't solve(matrix) it just grabs the solution from cache.
                                                 #if not it solves the matrix, then stores it in cache.
        
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