#' outcome_cat_table
#'
#' @param status this is a data set of visits that has been through the sort_date
#' function so that it a subset between two dates
#' @param demo This is the demographics data set of all the active employees
#' @param date1 lower bound date
#' @param date2 upper bound date
#' This function takes a visit data set and a demo graphics data set and returns
#' a table of sums
#'

outcome_cat_table = function(status, demo, date1, date2) {
  outcat = bind_cols(tibble(" " = c(
    "OUTBP",
    "OUTBG",
    "OUTCHOL",
    "OUTWEIGHT",
    "WELLNESS",
    "Tobacco Use"
  )),
  Totals= bind_rows(status %>%
                      filter(OUTBP==1) %>%
                      summarise(Totals=n_distinct(STUDYID)),
                    status %>%
                      filter(OUTBG==1) %>%
                      summarise(Totals=n_distinct(STUDYID)),
                    status %>%
                      filter(OUTCHOL==1) %>%
                      summarise(Totals=n_distinct(STUDYID)),
                    status %>%
                      filter(OUTWEIGHT==1) %>%
                      summarise(Totals=n_distinct(STUDYID)),
                    status %>%
                      filter(WELLNESS==1) %>%
                      summarise(Totals=n_distinct(STUDYID)),
                    demo %>%
                      summarise(Totals = sum(TOBACCO))
    )
  )
  names(outcat) = c(paste0(date1, " to ", date2), "Totals")
  return(outcat)

}




#' visit_table
#'
#' @param status subsetted visit dates
#' @param date1 lower date limit
#' @param date2 upper date limit
#' This is a function that takes a subsetted visit date set from sort date
#' and returns the number of visits and number of first visits in a table
#'


visit_table = function(status, date1, date2){
  out =bind_cols(" "=c("Number of Visits", "Number of First Visits"),
                      Totals=rbind.data.frame(t(pacer_status %>%
                                                    summarise(length(DATE),
                                                              sum(FIRSTVISIT)))
                      )
                 )
  names(out) = c(paste0(date1, " to ", date2), "Totals")
  return(out)
}
