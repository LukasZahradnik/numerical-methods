LeastSquares <- function(x, y) {
    # Args:
    #   x: The vector of x values of points.
    #   y: The vector of y values of points.
    #
    # Returns:
    #   The function of line.
    
    sum_x   <- sum(x)
    sum_x_y <- sum(x * y)
    sum_x_x <- sum(x * x)
    
    b <- (sum(y) - sum_x * sum_x_y / sum_x_x) / (-sum_x * sum_x / sum_x_x + length(x))
    a <- (sum_x_y - b * sum_x) / sum_x_x
    
    return(function(x) {
        return(a*x + b)
    })
}