getCofactors <-
function(M) {
    stopifnot(length(unique(dim(M)))==1) # Check if Matrix = Square
    cf <- M # creating a Matrix that has the same Dimensions as M 
    for(i in 1:dim(M)[1]){
        for(j in 1:dim(M)[2]){
            cf[i,j] <- (det(M[-i,-j])*(-1)^(i+j)) # overwriting the Values of cf Matrix with cofactors
        }
    }
    return(cf) # output of cofactors matrix
}
