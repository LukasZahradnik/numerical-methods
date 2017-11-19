MonteCarlo <- function(f, a, b, h, n) {
    #
    #
    # Args:
    #   f: The function for numerical integration.
    #   a: The left boundary of interval.
    #   b: The right boundary of interval.
    #   h: The upper boundary.
    #   n: The count of randomly generated numbers.
    #
    # Returns:
    #   The numerical integration of functin f on interval <a, b>
    
    x <- runif(n = n, min = a, max = b)
    y <- runif(n = n, min = 0, max = h)
    return( (b - a) * h * length(y[y < f(x)]) / n )
}

MonteCarloMean <- function(f, a, b, n) {
    #
    #
    # Args:
    #   f: The function for numerical integration.
    #   a: The left boundary of interval.
    #   b: The right boundary of interval.
    #   n: The count of randomly generated numbers.
    #
    # Returns:
    #   The numerical integration of functin f on interval <a, b>
    
    return((b - a) * sum(f(runif(n = n, min = a, max = b))) / n)
}