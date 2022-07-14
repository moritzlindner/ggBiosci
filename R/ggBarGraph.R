#' @importFrom ggpubr theme_pubr
#' @import ggplot2

ggBarGraph<-function(data,
         values_from,
         group_from,
         unit="",
         y_label=values_from,
         font_size=8){

  prefix<-get_si(median(abs(data[,values_from])))
  data[,values_from]<-data[,values_from] / (10 ^ get_exp_eng(si_to_exponent(prefix)))

  data<-GroupStats(data,
                   values_from,
                   group_from,
                   statfun = function(x) {
                     c(mean = mean(x),
                       se = sqrt(var(x) / length(x)),
                       n = length(x))
                   })

  data$n_pos<-unit(data$mean,"npc")-unit((font_size*1.3)/.pt,"npc")
  if(any(as.numeric(data$mean)<as.numeric(font_size*1.3*2/.pt))){
    data$n_pos[as.numeric(data$mean)<as.numeric(font_size*1.3*2/.pt)]<-
      unit(data$mean[as.numeric(data$mean)<as.numeric(font_size*1.3*2/.pt)]+
             data$se[as.numeric(data$mean)<as.numeric(font_size*1.3*2/.pt)],"npc")+
      unit((font_size*1.3)/.pt,"npc")
  }

  ggplot(data, aes(x = as.factor(Group),
                   y=mean,
                   ymin=mean-se,
                   ymax=mean+se,
                   label=n)) +
    geom_errorbar(width = 0.4, aes())+
    geom_bar(stat='identity')+
    geom_text(aes(y=as.numeric(n_pos)),size=font_size/.pt)  +
    ylim(0, NA) +
    theme_pubr(base_size = font_size) +
    ylab(paste0(y_label," [",prefix,unit,"]")) +
    xlab(NULL)

}

