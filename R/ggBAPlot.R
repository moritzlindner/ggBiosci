#' Bland-Altman plot
#'
#' Draws a Bland-Altman Plot using \link[ggplot2:ggplot]{ggplot2:;ggplot()},
#'
#' @param data Default dataset to use for plot - in a long format. If not already a data.frame, will be converted to one.
#' @param names_from,values_from Character strings indicating which column contains the group variable and the values, resp.
#' @param unit SI unit of the data in \code{values_from}
#' @return A ggplot
#' @seealso \link[ggplot2:ggplot]{ggplot2:;ggplot()}
#' @import ggplot2
#' @importFrom ggpubr theme_pubr
#' @importFrom tidyr pivot_wider
#' @export ggBAPlot
ggBAPlot<-function(data,
                   names_from,
                   values_from,
                   unit=""){
  data<-as.data.frame(data)
  grouplevels<-levels(data[,names_from])

  data<-as.data.frame(pivot_wider(data,names_from = names_from, values_from = values_from))
  data$mean<-apply(data[,grouplevels],1,mean)
  data$diff<-(data[,grouplevels[1]]-data[,grouplevels[2]])

  si_x<-get_si(max(abs(data$mean)))
  si_y<-get_si(max(abs(data$diff)))
  print(get_exp_eng(si_to_exponent(si_x)))
  print(data$mean)
  data$mean<-data$mean / (10 ^ get_exp_eng(si_to_exponent(si_x)))
  print(data$mean)
  data$diff<-data$diff / (10 ^ get_exp_eng(si_to_exponent(si_y)))


  ggplot(data=data,aes(x=mean,y=diff))+
    geom_point()+
    geom_hline(yintercept=0)+
    geom_hline(yintercept=+sd(data$diff),linetype = "dashed")+
    geom_hline(yintercept=-sd(data$diff),linetype = "dashed")+
    theme_pubr()+
    labs(x=paste0("Mean of ",grouplevels[1], " with ",grouplevels[2], "[",si_x,unit,"]"),
         y=paste("Difference ",grouplevels[1], "-",grouplevels[2]), "[",si_y,unit,"]")

}