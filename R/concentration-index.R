#' Return concentration index value of `hci` objects
#' @noRd
concentration_index <-
  function(object) {
    if (!any(class(object) == 'hci')) stop("Object is not of class hci")
    return(object$concentration_index)
}
