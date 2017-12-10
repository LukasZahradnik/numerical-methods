Simpson <- function(f, a, b, n = 1) {
    #
    #
    # Args:
    #   f: The function.
    #   a: The left bound.
    #   b: The right bound.
    #   n: Number of steps.
    #
    # Returns:
    #   Numerical approximation of definite integral.
    
    h <- (b - a) / n
    return((sum(f(seq(a + h, b - h, length.out=n - 1)) * rep(c(4,2), length.out=n - 1)) + f(a) + f(b)) * h / 3)
}