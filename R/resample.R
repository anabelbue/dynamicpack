

#' @title  Randomly changing the order of obersverations
#' @description  This function randomly resamples observations separately for variables and groups (e.g., individual participants)
#' so that meaningful information contained in the order of the observation gets lost, but each groups still has the same set of values.
#' @param data A data frame containing hierarchical data (e.g., time series data)
#' @param ID The grouping variable (e.g., to distinguish participants from each other)
#' @param variables A vector of variable names for which the order should be changed.
#' @examples
#' tib  <- dplyr::tibble(participant=rep(1:10, each=10),  # grouping variable
#' happiness = rnorm(100, mean = 3.5, sd=1.7),     # order should not be changed
#' stress = rnorm(100, mean = 2, sd= 1.3),         # order should be changed
#' anxiety = rnorm(100, mean = 1.7, sd= 2))        # order should be changed
#'
#' new_tib <- resample(tib, participant, c("stress", "anxiety"))
#'
#' @return The resampled data frame.
#' @export

resample <- function(data, ID, variables){
  if(!is.data.frame(data)){
    stop('This functions only works for data frames (including tibbles).\n',
         'You have provided an object of class: ', class(data))
  }
  resampled_data  <- data.frame()
  participants <- data %>% dplyr::select({{ ID }}) %>% dplyr::distinct() %>% dplyr::pull()
  for(p in participants){
    i_dat <- dplyr::filter(data, {{ ID }} ==  p)
    for(v in variables) {
      old_values <- dplyr::select(i_dat, {{ v }} ) %>% dplyr::pull()
      new_values <- sample(old_values)
      i_dat[v] <- new_values
    }
    resampled_data <- dplyr::bind_rows(resampled_data, i_dat)
  }
  return(resampled_data)
}

?tibble
