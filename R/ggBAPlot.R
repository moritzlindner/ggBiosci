#' Bland-Altman plot
#'
#' Draws a Bland-Altman Plot for visually analyzing the agreement between two different assays using \link[ggplot2:ggplot]{ggplot2:;ggplot()}.
#'
#' @param data Data set to use for ploting - in a long format. A data.frame.
#' @param names_from,values_from Character strings indicating which column contains the assay identifier and the values, resp.
#' @param unit SI unit of the data in \code{values_from}
#' @return A ggplot
#' @seealso \link[ggplot2:ggplot]{ggplot2::ggplot()}
#' @import ggplot2
#' @importFrom ggpubr theme_pubr
#' @importFrom tidyr pivot_wider
#' @export ggBAPlot
ggBAPlot <- function(data,
                     names_from,
                     values_from,
                     group_from = NULL,
                     unit = "") {
  if(!class(data)=="data.frame"){
    stop("'data' is not a data.frame")
  }
  namelevels <- levels(data[, names_from])

  suppressWarnings({
    data <-
      as.data.frame(pivot_wider(data, names_from = names_from, values_from = values_from))
  }, classes = c("warning", "message"))
  data$mean <- apply(data[, namelevels], 1, mean)
  data$diff <- (data[, namelevels[1]] - data[, namelevels[2]])
  if (is.null(group_from)){
    data$group<-TRUE
  }else{
    colnames(data)[colnames(data) == group_from] <- "group"
  }

  si_x <- get_si(max(abs(data$mean)))
  si_y <- get_si(max(abs(data$diff)))
  data$mean <- data$mean / (10 ^ get_exp_eng(si_to_exponent(si_x)))
  data$diff <- data$diff / (10 ^ get_exp_eng(si_to_exponent(si_y)))

  out <- ggplot(data = data, aes(x = mean, y = diff,
                                 if(length(unique(data$group))){ shape = group})) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = +sd(data$diff), linetype = "dashed") +
    geom_hline(yintercept = -sd(data$diff), linetype = "dashed") +
    theme_pubr() +
    labs(
      x = paste0(
        "Mean of ",
        namelevels[1],
        " with ",
        namelevels[2],
        "[",
        si_x,
        unit,
        "]"
      ),
      y = paste("Difference ", namelevels[1], "-", namelevels[2]),
      "[",
      si_y,
      unit,
      "]",
      color = group_from
    )

  out
}
