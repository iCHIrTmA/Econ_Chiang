Leontief <-
function(A,d){
    I <- diag(1, nrow=nrow(A), ncol=ncol(A));
    excess <- 1-colSums(A);
    IminusA <- I-A;
    sol_vals <- solve(IminusA) %*% d;
    req_input <- sum(sol_vals*excess);
    return(c(sol_vals, req_input))
    }
