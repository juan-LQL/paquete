#' Read a .csv file
#' 
#'
#' @param filename Filename (as character) were the .csv is located.
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @note Will stop the execution and return a message if the filename doesn't exist.
#' @return Returns a data frame tbl using dplyr::tbl_df (This function is deprectated)
#' @export
#' 
#' @examples 
#' fars_read("accident_2003.csv.bz2")
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Recives a year and returns a filename for the fars data
#'
#' @param year Year of the filename
#'
#' @return Returns a character vector of length 1 with the year added to the filename. 
#' @export
#' 
#' @examples 
#' make_filename(2003)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' This function recieves the years some filenames, reads them and returns the MONTH and year for each file
#' 
#' @importFrom dplyr mutate select
#' If any file is not read correctly, a warning for the specific year will be prompted, but the rest of files will be still read.
#' 
#' @param years Vector of years that the filenames have. 
#'
#' @return Returns a list of data frame tbls with two columns: MONTH and year
#' @export
#' 
#' @examples 
#' fars_read_years(2003:2005)
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

#' Summarises the number of elements each year by month of a list of data frame tbls
#' 
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' 
#' @param years Vector of years that the filenames have.
#'
#' @return Returns a tbl with year as columns and MONTH as the first column 
#' @export
#'
#' @examples 
#' fars_summarize_years(c(2003, 3004))
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Given a state numbers, the function makes a graph of this state
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @notes Will stop the execution if the state number is not found in the data. Will return a warning if there are no accidents in the data. 
#'
#' @param state.num The state number of the graph that will be plotted. (1 to 56)
#' @param year Year of the filename we want to read
#'
#' @return Returns a plotted map of the state with points were the accidents happened. 
#' 
#' @examples 
#' fars_map(5, 20013)
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
