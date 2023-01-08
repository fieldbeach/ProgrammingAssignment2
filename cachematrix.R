#The result of the below functions is the inverse of (only) an invertible matrix. 
#Coming from Python, I think of the first function like a class and the resulting 
#variable it's object. 
#Once the object is made, i.e. I run makeCacheMatrix and store it in a "object",
#I can retrieve the methods within that object when executing other functions.
#This is later done in the cacheSolve function.

#In the function below;
#have an empty input matrix as default value for the argument
#define m as null when "initializing the object",
#define a set function. Here we get the input matrix and we set m as null 
#otherwise the cached value would persist event though we passed a new input matrix
#get returns this input matrix
#setinverse assigns the value we calculate to m
#getinverse returns this value
#lastly we store each function in a name corresponding to the function name.
#this simplifies the process to call each function from the object we create

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#When we have initialized the object above, we can use it's "methods" i.e.
#functions in another function. This is what we do when calling cacheSolve.
#first we call getinverse in the matrix object. Hence, check if it's cached
#if this is not null, we can simply return the cached version of it
#if it is null, we get the input matrix, calculate the inverse, save it to m
#lastly we return m as well from the function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

#here is an example. 
#input matix:
input_matrix <- matrix(1:4, nrow = 2)
#initialize the object
matrix_obj <- makeCacheMatrix()
#using it's set method, set the value of the matrix
matrix_obj$set(input_matrix)
#looking at the data to see that it's stored properly:
print(matrix_obj$get())
#of course, we could have stored it when calling makeCacheMatrix
#since the matrix is stored in x, we can now run cacheSolve with the object as input.
cacheSolve(matrix_obj)
#this returns the inverse matrix upon being run. We can also see that its stored
#in the global environment by calling the getinverse function:
matrix_obj$getinverse()
#this also proves that it's cached. Running cacheSolve again triggers the if statement:
cacheSolve(matrix_obj)

