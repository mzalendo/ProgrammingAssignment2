#---------------------------------------------------------------------------------------#
# Matrix inversion is usually a costly computation and there may be some benefit to 	#
# caching the inverse of a matrix rather than computing it repeatedly. This program	#
# computes the inverse of the matrix by using functions that cache the results.		#
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------#
# Function name: makeCacheMatrix							#
#											#
# Function description:									#
# 	This function creates a special "matrix" object that can cache its inverse.	#
#	It performs the following operations:						#
#		1. set the value of the vector						#
#		2. get the value of the vector						#
#		3. set the value of the mean						#
#		4. get the value of the mean						#
#---------------------------------------------------------------------------------------#

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m
	
	# Insert the three functions defined above into a list object.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

} # --- End of function makeCacheMatrix ------


#---------------------------------------------------------------------------------------#
# Function name: cacheSolve								#
#											#
# Function description:									#
# 	This function takes an object as input.						#
#	The input object contains:							#
#		* The matrix to be transformed						#
#		* The function: get 							#
#		* The function: setinverse						#
#		* The function: getinverse						#
#	This function assumes that the input matrix is invertible.			#
#	If the inverse matrix is already computed, it returns that inverse matrix.	#
#	If the inverse is not computed yet, it computes the inverse and caches it.	#
#---------------------------------------------------------------------------------------#

cacheSolve <- function(x, ...) {
        invMatrix <- x$getinverse()		# Get cached inverse matrix
        if(!is.null(invMatrix)) {		# Check if cached inverse has a value or not
        	message("getting cached data")
                return(invMatrix)		# Return cached matrix, inverse of 'x'
        }
        data <- x$get()				# Get the matrix to be inversed 
        invMatrix <- solve(data, ...)		# Compute the inverse
        x$setinverse(invMatrix)			# Cache the inverse matrix
        invMatrix				# Return a matrix, the inverse of 'x' 

} # --- End of function cacheSolv   ------

#------------------------------------  END  --------------------------------------------#

