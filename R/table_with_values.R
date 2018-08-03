#' twv_single
#' This is a function that makes a table with values (twv) for a
#' single vector of RIsk values. Typically this is the last visit
#' of a time period
#'
#' @param vec is a vector of risk values
#'
#'



twv_single = function(vec){
  values = data.frame("Risk Level"=c("Low",
                                     "Moderate",
                                     "High",
                                     "Very High"),
                      "Totals"=rep(0,4))
  for(i in 1:4){
    values$Totals[i] = sum((na.omit(vec)==i)*1)
  }
  values = values %>%
    mutate(Frequency = round(Totals/ sum(Totals), digits=4))
  full_table = values %>%
    summarise(Risk.Level = "Totals",
              Totals = sum(Totals),
              Frequency = sum(Frequency)) %>%
    bind_rows(values, .)
  return(full_table)
}




#' twv_mult
#' @param current_vec vector of risk value from most current visit
#' @param previous_vec vector of risk value from previous visit
#' @param new_vec vector of risk values for New Employees
#'
#' This works in a similar manner as twv_singe, but it now makes tables with
#' current, previous, and new participant vectors

twv_mult = function(current_vec, previous_vec, new_vec=NULL){
  if (is.null(new_vec)){

    values = data.frame("Risk Level"=c("Low", "Moderate","High","Very High"),
                        "Previous"=rep(0,4),
                        "Current"=rep(0,4))

    tbl = bind_cols(Previous = previous_vec, Current = current_vec) %>%
      na.omit()

    for(i in 1:4){
      values$Previous[i] = sum((na.omit(tbl[,1])==i)*1)
      values$Current[i] =  sum((na.omit(tbl[,2])==i)*1)
    }

    values = values %>%
      mutate(Previous_Freq = round(Previous/ sum(Previous), digits=3)) %>%
      mutate(Current_Freq = round(Current/ sum(Current), digits=3))

    full_table = values %>%
      summarise(Risk.Level = "Totals",
                Current = sum(Current),
                Previous = sum(Previous),
                Previous_Freq = sum(Previous_Freq),
                Current_Freq = sum(Current_Freq)) %>%
      bind_rows(values, .)

    return(full_table[c(1,2,4,3,5)])

  } else {

  values = data.frame("Risk Level"=c("Low", "Moderate","High","Very High"),
                      "Previous"=rep(0,4),
                      "Current"=rep(0,4),
                      "New Participant" = rep(0,4))

  tbl = bind_cols(Previous = previous_vec, Current = current_vec) %>%
    na.omit()

  for(i in 1:4){
    values$Previous[i] = sum((na.omit(tbl[,1])==i)*1)
    values$Current[i] =  sum((na.omit(tbl[,2])==i)*1)
    values$New.Employee[i] = sum((na.omit(new_vec)==i)*1)
  }

  values = values %>%
    mutate(Previous_Freq = round(Previous/ sum(Previous), digits=3)) %>%
    mutate(Current_Freq = round(Current/ sum(Current), digits=3)) %>%
    mutate(New_Freq = round(New.Employee/ sum(New.Employee), digits=3))

  full_table = values %>%
    summarise(Risk.Level = "Totals",
              Current = sum(Current),
              Previous = sum(Previous),
              Previous_Freq = sum(Previous_Freq),
              Current_Freq = sum(Current_Freq),
              New.Employee = sum(New.Employee),
              New_Freq = sum(New_Freq)) %>%
    bind_rows(values, .)

  return(full_table[c(1,2,5,3,6,4,7)])
  }
}


#' twv_combo
#' @param current_vec current visit vector
#' @param previous_vec previous visit vector
#'
#' This function takes two  vectors of Risk values and creates a table of all
#' combinations of the risk values for each visit
#'

twv_combo = function(current_vec, previous_vec){
  values = bind_cols(Previous = previous_vec, Current = current_vec) %>%
    na.omit() %>%
    count(Previous, Current) %>%
    mutate(Current = recode(Current,`1`="Low",`2`="Moderate",`3`="High",`4`="Very High")) %>%
    mutate(Previous = recode(Previous,`1`="Low",`2`="Moderate",`3`="High",`4`="Very High"))


  values = values %>%
    mutate(Percent = round(n/ sum(n), digits=4)) %>%
    mutate(`Cumulative Freq` = cumsum(n)) %>%
    mutate(`Cumulative Percent` = cumsum(Percent))
  return(values)
}
