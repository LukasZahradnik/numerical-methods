GaussElimination <- function(A, b) {
    # Args:
    #   A: The matrix of left side.
    #   b: The vector of right side.
    #
    # Returns:
    #   The vector of solution for given A and b.
    
    n <- length(b)
    
    for(i in 1:(n - 1)) {
        for(j in (i + 1):n) {
            k <- -A[j, i]/A[i, i]
            b[j] <- b[i] * k + b[j]
            A[j, (i:n)] <- A[i, (i:n)] * k + A[j, (i:n)]
        }
    }
    
    x <- b
    x[n] <- b[n]/A[n,n]
    for(i in (n-1):1) {
        x[i] <- (b[i] - sum(A[i, (i+1):n] * x[(i+1):n]))/A[i,i]
    }
    
    return(x)
}