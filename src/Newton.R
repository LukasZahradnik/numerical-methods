Newton <- function(f, d, x) {
    #
    #
    # Args:
    #   f: The function.
    #   d: The derivation of f function.
    #   x: The starting point for root search.
    #
    # Returns:
    #   The x coord of root.
    
    while(TRUE) {
        tmp <- x - f(x)/d(x)
        if(abs(x - tmp) < 5e-16)
            return(x)
        x <- tmp
    }
}
