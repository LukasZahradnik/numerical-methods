FixedPoint <- function(f, a, tolerance = 10e-10) {
    # Args:
    #   f: The function
    #   a: The starting x coord
    #   tolerance: Treshold to stop
    #
    # Returns:
    #   The root of the function f
    
    while(TRUE) {
        x <- f(a)
        if(abs(x-a) < tolerance) break
        a <- x
    }
    
    return(x)
}