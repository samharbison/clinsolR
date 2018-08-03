#' loadDemo
#'
#' @param dir This is the directory containg the data to load in
#'
#' This is a fucntion that loads in demographics data set for a given company
#' The function takes the directory path way to the given company you would
#' like to load data for.
#'
#'
#'
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import tibble
#' @import stringr
#' @import forcats
#' @import glue
#'
#' @export


loadDemo <- function(dir) {
  demo <- read_csv(paste0(dir,"tblDemo.csv"))
  demo %<>% replace_na(list(ACTIVE = 0,
                            TOBACCO = 0,
                            SPOUSEDEP = 0))
  demo$DATESIGNED <- as.Date(demo$DATESIGNED, format = "%m/%d/%y")
  return(demo)
}




#' loadStatus
#'
#' @param dir path to directory
#' THis is a function that takes a directory path and returns the visit dataset
#' for a company in question.
#'
#' @export


loadStatus <- function(dir){
  status <- read_csv(paste0(dir,"tblStatus.csv"))
  status$DATE <- as.Date(status$DATE, format = "%m/%d/%y")
  status %<>% replace_na(list(OUTBP = 0,
                              OUTBG = 0,
                              OUTCHOL = 0,
                              OUTWEIGHT = 0,
                              WELLNESS = 0))
  return(status)
}
