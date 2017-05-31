#' changeDates
#'
#' change statlink dates to R Date
#'
#' change statlink dates to R Date.
#'
#' @param date a date character vector containing "m" or "q".
#'
#' @author Bo Werth
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @export
#' @examples
#' changeDates(c("2012m1", "2012m2"))
#' changeDates(c("2012q1", "2012q2", "2012q3", "2012q4"))

changeDates <- function(idx){
  idx_copy <- as.character(idx)

  if(all(str_detect(idx_copy, "m"))) {

    idx_date_list <-
      strsplit(idx_copy, split = "m")

    year <- sapply(idx_date_list, "[[", 1)
    month1 <- sapply(idx_date_list, "[[", 2)
    month_pad <-
      str_pad(month1, width = 2, side = "left", pad = "0")

    idx_date <-
      as.Date(paste(year, month_pad, "01", sep = "-"))

  } else if(all(str_detect(idx_copy, "q"))) {

    idx_date_list <-
      strsplit(idx_copy, split = "q")

    year <- sapply(idx_date_list, "[[", 1)
    quarter1 <- sapply(idx_date_list, "[[", 2)

    quarter2 <- as.numeric(quarter1) * 3 - 2
    quarter_pad <-
      str_pad(quarter2, width = 2, side = "left", pad = "0")

    idx_date <-
      as.Date(paste(year, quarter_pad, "01", sep = "-"))

  } else {
                                        #try straight as.Date anyway
    idx_date = tryCatch({
      as.Date(idx_copy)
    }, error = function(e) {
      stop('Unrecognized time format and no frequency. Unable to convert to Date.')
    })
  }
  return(idx_date)
}

## ## using dplyr
## dat2 <-
##   dat %>%
##   tidyr::separate(date, c("year", "month"), sep = "m") %>%
##   dplyr::mutate(month2 = str_pad(month, width = 2, side = "left", pad = "0"),
##                 day = "01") %>%
##   tidyr::unite(col = "date", from = c(year, month2, day), sep = "-") %>%
##   dplyr::mutate(date = as.Date(date)) %>%
##   select(-month)
