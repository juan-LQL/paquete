#' Read a Fatality Analysis Reporting System file
#'
#' @param filename A string containing the path and name of the file
#'
#' @return A data frame containing the table in the CSV file
#' @export
#'
#' @examples
#' fars_2013 <- fars_read(filename = 'accident_2013.csv')
#' head(fars_2013)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create the filename according to a specified year
#'
#' @param year Integer that indicates the year of the data
#'
#' @return A string containing the name the file should have.
#'
#' @examples
#' fname <- make_filename(2020)
#' print(fname)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read all FARS files corresponding to the specified years
#'
#' @param years Vector of size 1 or bigger with the wanted years to be read
#'
#' @return A list of length equal to \code{length(years)}, each element containing
#' a tibble with the variables \code{MONTH} and \code{year}, or \code{NULL} if the
#' file cannot be found
#'
#' @examples
#' fars_read_years(c(2013, 2014))
#'
#' @importFrom dplyr mutate select
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Count ocurrences by year and month
#'
#' @param years
#'
#' @return A tibble with columns \code{MONTH} and one for every element in \code{years}
#' summarizing how many elements are in the data for each month and year.
#'
#' @examples
#' fars_summarize_years(c(2013, 2014))
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @note The function will return an error if dplyr::n() is no used instead of just n()
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map the location of all FARS that occurred in a given state and year
#'
#' @param state.num Integer representing the number of the state to be mapped.
#' If the number entered is not valid, an error is returned
#' @param year Integer that refers to the year to be mapped
#'
#' @return This function is used for its side-effect.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(1,2013)
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
