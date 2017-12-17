Spline <- function() {
    #WIP
}


TridiagonalMatrix <- function(a, b, c, d) {
    #
    #
    # Args:
    #   a: Elements on the main diagonal
    #   b: Elements on the diagonal above the main diagonal
    #   c: Elements on the diagonal under the main diagonal
    #   d: 
    #
    # Returns:
    #   
    
    n <- length(a)
    
    for(i in 2:n) {
        tmp <- b[i - 1] / a[i - 1]
        a[i] <- a[i] - tmp * c[i - 1]
        d[i] <- d[i] - tmp * d[i - 1]
    }
    
    d[n] <- d[n] / a[n]
    for(i in (n-1):1) {
        d[i] <- (d[i] - c[i] * d[i + 1]) / a[i]
    }
    
    return(d)
}