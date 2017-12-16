Lagrange <- function(x, y, act) {
    #
    #
    # Args:
    #   x: The array of points x coords to interpolate.
    #   y: The array of points y coords to interpolate.
    #   act: The x coords to calculate y for.
    #
    # Returns:
    #   The y coord for given x (act) value

    n <- length(x)
    res <- 0
    
    for(i in 1:n) {
        tmp <- 1
        for(j in setdiff(1:n, i)) {
            tmp <- tmp * ((act - x[j])/(x[i] - x[j]))
        }
        res <- res + y[i] * tmp
    }
    
    return(res) 
}