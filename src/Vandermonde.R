Vandermonde <- function(x, y) {
    # Args:
    #   x: The x coords.
    #   y: The y coords.
    #
    # Returns:
    #   The function of interpolation
    
    n <- length(x)
    A <- matrix(1,n,n)
    
    for(i in 2:n) {
        A[,i] <- A[,i-1] * x
    }
    
    return(solve(A, y))
}