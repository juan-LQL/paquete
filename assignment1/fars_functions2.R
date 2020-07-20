#' @title  Read FARS data
#'
#' @description This function takes a filename as input, reads it using the readr::read_csv function, and 
#' saves it as a dplyr tbl_df.
#' 
#' @param filename
#'
#' @details This will only work if you input the full filename (referenced from your
#' current working directory) and if the file is in .csv format.
#' 
#' @examples require(readr)
#' require(dplyr)
#' 
#' Input Sources
#' fars_read(foo.csv)
#' 
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title  Read FARS filename
#'
#' @description This function takes a year as input and creates a filename that lists the
#' accidents from that year using FARS templates
#' 
#' @param year
#'
#' @details This will only work if you input a four-digit year
#' 
#' @examples
#' 
#' Input Sources
#' fars_read(2015)
#' 
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title  Read FARS data for a given set of years
#'
#' @description This function takes a vector of years as input and reads all relevant data
#' into a dataframe
#' 
#' @param filename
#'
#' @details This will only work if you input the full filename (referenced from your
#' current working directory) and if the file is in .csv format.
#' 
#' @examples require(readr)
#' require(dplyr)
#' 
#' Input Sources
#' fars_read_years(2015:2017)
#' 
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
#' @title  Read and summarize FARS data
#'
#' @description This function takes several years as an input and outputs summarization of
#' those years
#' 
#' @param years
#'
#' @details This will only work if you input the full four-digit years and those years
#' are present in your dataset
#' 
#' @examples require(readr)
#' require(dplyr)
#' 
#' Input Sources
#' fars_summarize_years(years)
#' 

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' @title Map FARS data
#'
#' @description This function takes a state number and year and maps accident data
#' 
#' @param state.num
#' @param year
#'
#' @details This will only work if you use the correct state numbers and have access to 
#' the info for the relevant years
#' 
#' @examples require(readr)
#' require(dplyr)
#' 
#' Input Sources
#' fars_read(foo.csv)
#' 
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