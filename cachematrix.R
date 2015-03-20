## Lexical Scoping helps us understand the sequence in which an “unknown” variable in a multi-function program will be used.
## 
## What happens when multi functions have to be resolved- two different types of environments come in play.
## The function environment, in cacheSolve is like a temporary workspace and results are cleaned after the session
## the parent environment, where the results are 'set', is like a permanent data environment.

## The way R handles calculating Function within functions flows as-
## 1. An initial cache of results is 'set' in the parent environment of the top level function- makeCachMatrix
## 2. While the structure for inverse and Matrix is set up in the parent environment, it is in the funtion
##  CacheSolve where the inverse is being computed.
## 3. The results from CacheSolve, replace the existing Null results in the parent environment
## 4. The use of '<<' enables the replacement of the Null results in I by the I << solve in cachSolve.
## 5. The set/get functions used with the above are instructions to set, get and update the files in the parent env
## 6. from running through other examples, I found that the value is more significant as iterations increase


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL                                 # creating a null matrix to hold the Inverse value
    print(environment())                     # testing the environment for f(x)
    evn <- environment()
    print(parent.env(evn))                   # for I a free variable the lexical scoping
                                             # will bind I values to existing values in the parent env.
    set <- function(y) {                     # defining another function set within f(x)
      x <<- y                                # which sets the value of y to x and resets I to null
      I <<- NUll
}
get <- function() x                         # creating a number of functions to be used in the second part
setinv <- function(solve) I <<- solve       # 'solve' is the function that computes the inverse of a matrix 
getinv <- function() I                      # Creating functions to set or get the inverse values from parent env
getevn <- function() environment()          # identify the environment for get function (this will differ from evn)
list(set=set, get= get,               
     setinv = setinv,
     getinv = getinv,
     getevn = getevn)                       # creating a list of functions which will be available in the parent env.

## there are two functions in this program. makeCachematrix, creating the defaults or "caching" initial values
## for the function f(x), next we are creating a function that is solving for the inverse of matrix X.   

cacheSolve <- function(x, ...) {
                                          ## Return a matrix that is the inverse of 'x'
  I <- x$getinv()                          # setting the default value or the recently created value saved to the parent  env                                             
  if (!is.null(I)) {
    message("getting cached data")          # if the most recent value is not null then
    return(I)                               #  returning I matrix from previous cacheSolve iteration
  }
  data <- x$getinv()                       # if the calculated value in cache is NULL then getting data
  I <- solve(data, ...)                    # solving inverse of data matrix, retaining other solve arguments(...) 
  x$setinv(I)                              # re-setting the new I value to the setinv matrix I in the parent env
  I                                        # returns the new calculated value for I the inverse matrix
  getevn<- function() environment()        # to better understand lexical scoping, identifying the env of new I matrix  
  
}
}
