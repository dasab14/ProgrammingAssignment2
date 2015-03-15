## The below two functions viz. makeCacheMatrix and cacheSolve have the capability to take any matrix as 
## input,solve the inverse,cache them and display the same for the set() input matrix. Additionally( although not asked in the exercise) the two functions together would also
## identify a non square matrix input and display a message pertaining to the same thus indicating that the 
## inverse cannot be calculated.

## The below function returns 5 child functions from inside its frame. The 5 are briefly described below.
## The set function can be used to enter a matrix by user.
## The get function helps in getting the value of the matrix as set by the set function, as and when required by any other function call.
## The cacheinvmat function just caches the inverse of  the user inputed matrix.
## The getcacheinvmat function can be called from another function to get the cached inverse matrix of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
	      sqmatrix <<-NULL															## This parameter will help determine whether the matrix is square
                list( set = function(y=matrix()){									## Generates a list with 5 functions
                          invmat<<-NULL    
                          x<<-y
                          },
                          get = function() x,
                          cacheinvmat = function(invmatsolve) invmat<<-invmatsolve, ## Using lexical scoping rules and <<- operator for free variable assignment and use
                          getcacheinvmat = function() invmat,
                          chksqmatrix= function(){									## This function helps to determine whether the matrix given as user input is square or not
                                    if(ncol(x)==nrow(x)){
                                              sqmatrix<<-NA
                                    }
                                    sqmatrix
                          }
                          )

}


## The function below takes arguments from the function above and does the following things.
## Checks to see if the input matrix is indeed a square matrix for inverse to be calculated.
## If the matrix is found to be square then the inverse if calculated and displayed only in case that there is no cached inverse matix for the input matrix. Otherwise it goes ahead in the function and just fethches and displays the previously cached value for the matrix.

cacheSolve <- function(x, ...) {
	z<-x$chksqmatrix()
          if(is.null(z)){															## Identifying to see if the matrix inputted is indeed a square matrix or not.
                    return(message("Input is not a Square Matrix"))
          }
          c<-x$getcacheinvmat()
          if(!is.null(c)){
                    message("Getting Cached Matrix")
                    return(c)                    
          }
          matrixdata<-x$get()
          invmatsolve<- solve(matrixdata,...)										## Solving for Inverse of matrix.
          x$cacheinvmat(invmatsolve)
          invmatsolve
        ## Return a matrix that is the inverse of 'x'
}
