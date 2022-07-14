#' Group statistics
#'
#' Calculates groups statistis
#'
#' @param data Data set to use - in a wide format. If not already a data.frame, will be converted to one.
#' @param values_from,group_from Character strings indicating which column contains the values and the group variable(s), resp. \var{group_from} can be a scalar or a vector.
#' @param statfun function to use for calculation of summary statistics. if returning more than one value, must be in a list.
#' @return A \link[base:ggplot]{ggplot2::ggplot()}
#' @import ggplot2
#' @importFrom stats aggregate
#' @importFrom tidyr pivot_wider
#' @export GroupStats
GroupStats <- function(data,
                        values_from,
                        group_from,
                        statfun = function(x) {
                          c(mean = mean(x),
                            se = sqrt(var(x) / length(x)),
                            n = length(x))
                        }) {
  data <- as.data.frame(data)
  if (!(all(c(values_from, group_from) %in% colnames(data)))) {
    stop(paste(
      "At least one of",
      values_from,
      values_from,
      "not found as columns in",
      deparse(substitute(data))
    ))
  }
  if (!(is.numeric(data[, values_from]))) {
    stop(paste(values_from, "is not numeric"))
  }
  if (length(values_from) > 1) {
    stop("This function can only operate on one variable column.")
  }

  if(length(group_from)>1){
    grouplist <- lapply((data[, group_from]), as.factor)
  }else{
    grouplist <- list(data[, group_from])
    names(grouplist)<-group_from
  }

  OUT <- aggregate(data[, values_from],
                   grouplist,
                   statfun)

  OUT <- cbind(OUT, OUT$x)

  OUT$x <- NULL
  return(OUT)
}
