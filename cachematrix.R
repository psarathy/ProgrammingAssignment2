## This assignment helps us understand the Lexical Scoping. How does R handle an “unknown” variable in a multi-function program will be used.
## 
## What happens when multi functions have to be resolved? Two different types of environments come into play.
## Traditionally, the logic works sequentially, in Lexical scoping it is recursive.
## The 'inner function' environment, in cacheSolve is like a temporary workspace and results are cleaned after the session
## The parent environment, makeCacheMatrix, where results are 'set or intiallized', is like a permanent data environment.

## The way R handles calculating Function within functions, in this example, is-
## 1. An initial cache of results is 'set' in the parent environment of the top level function- makeCachMatrix
## 2. While the structure/ Environment for inverse and Matrix is set up in the parent environment, it is in the funtion
##  CacheSolve, where the inverse is actually being computed.
## 3. The results from 'Solve', replace the existing Null results in the parent environment,
## 4. using '<<'. This enables the replacement of the Null results in I by the I << solve in cachSolve.
## 5. The set/get functions used with the above are instructions to set, get and update the files in the parent env
## 6. from running through other examples, I found that the value is more significant as iterations increase.


makeCacheMatrix <- function(x = matrix()) {  # Creating a parent function which receives a matrix as its input
    I <- NULL                                # creating a null matrix I to hold the Inverse value
    #print(environment())                     # testing the environment for f(x), will help us understand the concept of 
    evn <- environment()                     # Lexical scoping
    #print(parent.env(evn))                   # for I a free variable the lexical scoping
                                             # will bind I values to existing values in the parent env.
    set <- function(y) {                     # Defining another function set within f(x)
      x <<- y                                # which sets the value of y to x and resets I to null
      I <<- NUll                             # Remember x was defined in theparent environment, while Y in the function env.
}
get <- function() x                         # Creating functions calling from the parent env and being defined under function env
setinv <- function(solve) I <<- solve       # 'solve' function computes the inverse of a matrix and assigns to I 
getinv <- function() I                      #  The new I is assigned to 'getinv' 
getevn <- function() environment()          # identify the environment for get function (this will differ from evn)
list(set=set, get= get,               
     setinv = setinv,
     getinv = getinv,
     getevn = getevn)                       # creating a list of functions which will be available in the parent env.

## there are two functions in this program. makeCachematrix, creating the defaults or "caching" initial values
## for the function f(x), next we are creating a function that is solving for the inverse of matrix X.   

cacheSolve <- function(x, ...) {           # CacheSolve is also a function of x defined in the parent env.
                                           #  When this function is run
  I <- x$getinv()                          # It seeks out the value from cache created by makeCacheMatrix actions                                             
  if (!is.null(I)) {
    message("getting cached data")          # if the most recent value is not null then
    return(I)                               #  returning I matrix from previous cacheSolve iteration and printing message
  }
  data <- x$getinv()                       # if the calculated value in cache is NULL then gets data
  I <- solve(data, ...)                    # solves for inverse using the data, retaining other solve arguments(...) 
  x$setinv(I)                              # re-sets the new I value to the setinv matrix in the parent env
  I                                        # The new calculated value for I the inverse matrix, is returned
  getevn<- function() environment()        #  comparing getevn and evn youcan understand as the program is run sequentiall
                                           # how variables are created across environments using Lexical scoping.
}
}
