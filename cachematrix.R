#These 2 functions calculate the inverse of matrix. The use special "matrix" which is a list of
#functions. This object holds the inital matrix and it's inverse if it has been calculated. 
#List also contains 2 functions which are used for changing data manually, either the matrix or 
#it's inverse. The second functions is used to calculate the inverse. If it has already been 
#calculated, then it is pulled from cache, i.e. the function that returnes the inverse from 
#special "matrix" is called.

#Argument of this function is a matrix x. function creates the special "matrix", which is
#actually list, that contains 4 functions. 2 of these functions are used to get inital matrix x
#and it's inverse. The other 2 are used for changing matrix and it's inverse. When this function
#is called for first time the inverse of matrix is NULL(because it hasn't yet been calcualated).
#We declare function set(), which is used to change initial matrix. It uses the "<<-" operator,
#which is assign operator. It searches through parent environments for an existing definition
#of the variable being assigned. The get() function is used for retrieving initial matrix. 
#setInverse() is used for manually setting inverse. The getInverse() is same as get() function,
#just for inverse.

makeCacheMatrix <- function(x = matrix()) {
    I<-NULL         #The inverse has not yet been calculated, so we assign in NULL value.
    set<-function(y){
        x<<-y       #Operator #<<-" searches for x in parent enviroment and assigns value y to it.
        I<<-NULL    #If you change the initial matrix, thr inverse changes.
    }
    get<- function() x
    setInverse<-function(inverse) I<<-inverse #Assigning value of inverse in parent enviroment.
    getInverse<-function() I
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) #The final list.
}


#Argument of this function is special "matrix" created in above function, and some other agruments
#passed with "..." which are used in function solve(). This function calculates inverse of matrix.
#If the inverse has not yet been calculated, the the function finds the inverse, and stores it.
#The special "matrix" is beeing used which contains the initial marix and it's inverse. If 
#inverse has already been calculated, then it is pulled from memory ("Cache"). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I<-x$getInverse() #We look if inverse has been calculated.
    if(!is.null(I)){  #If inverse has been calculated, if() statement is true, and inverse is
        return(I)     # pulled from cache. If it hasn't been calculated the NULL value is 
    }                 # returned, and if() statement is false.
                #This block of code is ran only if inverse has not been calculated yet.
    data<-x$get()     #The initial matrix is assigned to variable data.
    I<-solve(data,...)#Inverse is found using solve() function
    x$setInverse(I)   #Inverse is storred in special "matrix"
    I                 #Inverse is returned.
}