Bisection <- function(f, a, b) {
    #
    #
    # Args:
    #   f: The function
    #   a: The left bound
    #   b: The right bound
    #
    # Returns:
    #   The root of the function f
    
    if(f(a) * f(b) < 0) {
        while(TRUE) {
            c <- (a + b) / 2
            if(c == a || c == b) return(c)
            ifelse(f(a) * f(c) < 0, b <- c, a <- c)
        }
    }
    
    return(FALSE)
}