#' demo_visit
#'
#' @param demo demographics data set
#' @param status visits data set
#' @param spouse True or False. Determines if the list returned contains spouse info
#' This function takes a demographics data set, a visit data set and creates a list
#' of the active employees and spouses. Spouses only returned it spouses parameter
#' is equal to TRUE
#' @export

demo_visit = function(demo, status, spouse = TRUE){
  if (spouse == TRUE){
    empl = demo %>% filter(ACTIVE==1, SPOUSEDEP==0)
    spouse = demo %>% filter(ACTIVE==1, SPOUSEDEP==1)
    ev = status %>% semi_join(empl, by = "STUDYID")
    sv = status %>% semi_join(spouse, by = "STUDYID")
    return(list(
      "Employee_Demo" = empl,
      "Spouse_Demo" = spouse,
      "Employee_Visit" = ev,
      "Spouse_Visit"=sv
    ))}
  else {
    empl = demo %>% filter(ACTIVE == 1)
    ev = status %>% semi_join(empl, by="STUDYID")
    return(list(
      "Employee_Demo" = empl,
      "Employee_Visit" = ev
    ))
  }
}

#' sort_date
#' @param status visit data set. Should be the visit dataset from the out of demo_visit
#' @param date1 lower boundary of date range
#' @param date2 Upper boundary of date range
#'
#' This is a function that takes a visit dataset (typically one that has been pushed through
#' the demo_visit function to only include active employees and spouses) returns a filtered
#' subset that is betweem the two date ranges. Both dates must be supplied.
#' @export
sort_date <- function(status, date1, date2) {
  stat_subset = status %>%
    filter(DATE > date1 & DATE <= date2) %>%
    arrange(DATE)
  return(stat_subset)
}
