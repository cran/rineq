#' Plots the concentration curve for an `hci` object.
#' @param x  Object with of `hci`
#' @param ... Further arguments passed to `base::plot()`
#' @return Invisibly returns `x` as the function is called for side effects (plotting). 
#' @importFrom graphics lines
#' @export
#' 
#' @examples
#' data(housing)
#' ci.bmi <- ci(ineqvar = housing$income, outcome = housing$bmi, method = "direct")
#' plot(ci.bmi)
#'  
plot.hci <-function(x, ...) {
    if (!inherits(x,'hci')) stop("Object is not of class hci")
  
    o <- order(x$ineqvar)
    xval <- x$fractional_rank[o]
    yval <- c(0,cumsum(x$outcome[o]) / sum(x$outcome))
    
    # shift xval to be 1, add 0 , (move from mid to endpoints)
    # fractional rank is given by (2*r_i - 1)/(2n), with min and max = 1/(2n), 1-(1/2n), resp. 
    # thus we can just shift it to be 1 by 1/(2n) to get the vanilla rank r_i for individual i 
    # see Eyrregers, Clarke, Van Ourti, 2012
    xval <- c(0,xval +  (1-xval[length(xval)]))
    
    
    plot(c(0,100), c(0,100), type = "l", main = "Concentration curve",  col = "black", ... , 
         xlab = paste0("Cumulative % ranked inequality"), 
         ylab = paste0("Cumulative % outcome"))
    lines(xval*100, yval*100, col = "darkred", lty = "dashed")
    
    # return X since its a base-style plot function (https://adv-r.hadley.nz/functions.html) called for side effects    
    invisible(x)
}

