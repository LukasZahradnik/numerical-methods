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
            k <- -A[j, i]/A[i, i]
            b[j] = b[i] * k + b[j]
            A[j,] = A[i,] * k + A[j,]
        }
    }
}