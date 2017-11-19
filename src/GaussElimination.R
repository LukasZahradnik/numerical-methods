GaussElimination <- function(A, b) {
    # Computes the sample covariance between two vectors.
    #
    # Args:
    #   A: The matrix of left side.
    #   b: The vector of right side.
    #
    # Returns:
    #   The solution for given A and b.
    
    n <- length(b)
    
    for(i in 1:(n - 1)) {
        for(j in (i + 1):n) {
            k <- -A[i + 1, 1]/A[i, 1]
            b[i + 1] = 
            
            
            
        }
       
        
        for(j in 1:ncol(A)) {
            A[i + 1, j] <- A[i, j] * k + A[i + 1, j]
        }
    }
}