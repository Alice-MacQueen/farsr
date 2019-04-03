#' @title Read Fatality Analysis Reporting System data
#'
#' @description \code{fars_read} reads in Fatality Analysis Reporting System (FARS) data
#'for a given \code{filename}, if the file exists.
#'
#' @usage fars_read(filename)
#'
#' @param filename The name of the FARS data file to read.
#' @param path The path to the FARS data file to read. The default is the path
#' to the three supplied raw data files.
#'
#' @return A table of FARS data.
#'
#' @examples
#' \dontrun{fars_read(filename = "accident_2013.csv.bz2")}
#' \dontrun{fars_read(filename = "accident_2014.csv.bz2"))}
#' \dontrun{fars_read(filename = "accident_2015.csv.bz2")}
#' \dontrun{fars_read(filename = "file_does_not_exist.csv", path = "bad/path")} # Results in an error
#'
#' @import readr
#'
fars_read <- function(filename, path = "inst/extdata") {
  if(!file.exists(file.path(path, filename)))
    stop("file '", filename, "' does not exist on this path")
  if(path == "inst/extdata"){
    data <- suppressMessages({
      readr::read_csv(system.file("inst/extdata", filename, package = "farsr"),
                      progress = FALSE)
  })
  } else{
  data <- suppressMessages({
    readr::read_csv(file.path(path, filename), progress = FALSE)
  })
  }
  dplyr::tbl_df(data)
}

#' @title Make a Fatality Analysis Reporting System file name.
#'
#' @description \code{make_filename} creates a Fatality Analysis Reporting System
#' (FARS) filename for a given four digit \code{year}.
#'
#' @usage make_filename(year)
#'
#' @param year The year you want to make a FARS filename for.
#'
#' @return A filename for a file containing FARS data.
#'
#' @examples
#' \dontrun{make_filename(year = 2014)}   # Makes FARS filename for the year 2014.
#' \dontrun{make_filename(year = 14)}     # Will not make a working FARS filename.
#'
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title Read in one or more years of FARS data.
#'
#' @description Reads in Fatality Analysis Reporting System data for one or more
#' years.
#'
#' @usage fars_read_years(years)
#'
#' @param years A year or vector of four-digit years for which you want to read
#' in FARS data.
#'
#' @return  A table or list of tables of FARS data.
#'
#' @examples
#' \dontrun{fars_read_years(years = 2014)}   # Returns FARS data for the year 2014.
#' years <- c(2013, 2014, 2015)
#' \dontrun{fars_read_years(years = years)} # Returns a list of three tables of FARS data.
#' \dontrun{fars_read_years(years = 14)}    # Results in an invalid year error.
#'
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

#' @title FARS Monthly Fatality Summaries
#'
#' @description Creates summaries of monthly fatalities using Fatality Analysis
#' Reporting System data for a specified year or years.
#'
#' @usage fars_summarize_years(years)
#'
#' @param years The year or years to get monthly summaries of fatalities for.
#'
#' @return A summary table of monthly fatalities for each year of FARS data.
#'
#' @examples
#' \dontrun{fars_summarize_years(years = 2014)}   # Monthly fatality summaries for 2014.
#'years <- c(2013, 2014, 2015)
#' \dontrun{fars_summarize_years(years = years)}     # Summary table for 2013-2015.
#' \dontrun{fars_summarize_years(years = 14)}        # Will return an error.
#'
#' @import readr tidyr magrittr dplyr
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title Plot FARS fatalities for a state and year.
#'
#' @description Makes a plot of Fatality Analysis Reporting System (FARS)
#' data for a given state number and year.
#'
#' @usage make_filename(year)
#'
#' @param year The year to be plotted
#' @param state.num The integer number of the state to be plotted, from 1-56.
#'
#' @return A maps object.
#'
#' @examples
#' \dontrun{fars_map_state(state.num = 10, year = 2014)}     # Returns a map for state 10.
#' \dontrun{fars_map_state(state.num = 100, year = 2014)}     # Returns an error.
#'
#' @import dplyr readr maps graphics
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- farsr::make_filename(year)
  data <- farsr::fars_read(filename)
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

