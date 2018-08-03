#' bar_charter
#'
#' @param tab table from twv_single or twv_mult function
#' @param var_title The title of the plot, an example would be "Blood Pressure"
#' @param doublebar This is an indication that there will be dual bars, and that the table
#' needs to be reformatted to a long version
#' @param dual_labels c("label1","label2") where these are the names for the legend
#'
#' This makes bar charts for each risk value
#' @export
#' @import ggplot2
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import tibble
#' @import stringr
#'
#'


bar_charter = function(tab, var_title, doublebar = FALSE, dual_labels = NULL ) {
  if (doublebar == TRUE) {
    t = tab[,c(1,3,5)] %>%
      gather(key, value, -Risk.Level)

    p = ggplot(t, aes(x=Risk.Level, y=value, fill=key))+
      geom_bar(stat="identity",
               position=position_dodge())+
      scale_x_discrete(limits=c("Low","Moderate","High","Very High"))+
      xlab("Risk Level") +
      ylab("Frequency") +
      ggtitle(paste0(var_title," Risk Employees")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_discrete(name = "Visit",
                          labels = dual_labels) +
      geom_text(aes(label=value),
                vjust=1.6,
                color="black",
                position = position_dodge(0.9),
                size=3.5)

    return(p)

  } else {
    t = tab

    p = ggplot(t, aes(x=Risk.Level, y=Frequency)) +
      geom_bar(stat="identity", position=position_dodge(), fill = "dodgerblue3") +
      scale_x_discrete(limits=c("Low","Moderate","High","Very High")) +
      xlab("Risk Level") +
      ylab("Frequency") +
      ggtitle(paste0(var_title," Risk Employees")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label=Frequency),
                vjust=1.6,
                color="black",
                position = position_dodge(0.9),
                size=3.5)

    return(p)
  }



}
