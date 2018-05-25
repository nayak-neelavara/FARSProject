#' Read NHTSA FARS data file
#'
#' Reads a Fatality Analysis Reporting System (FARS) data file in CSV format and creates a tidyverse tibble from it.
#'
#' @param filename A path to a file or a connection.
#' Compressed files in with  .gz, .bz2, .xz, or .zip formats supported.
#'
#' @return Data frame.
#'
#' @examples
#' \dontrun{
#' fars_read("./data/accident_2013.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Get the name of NHTSA FARS data file for a given year
#'
#' Makes a filename, without the full path, of a Fatality Analysis Reporting System (FARS) data file
#' in CSV format for a specified year.
#'
#' @param year Year of observations.
#'
#' @return A character vector containing the name of the file, without full path.
#'
#' @examples
#' \dontrun{
#' make_filename(2015)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Get month and year variables from the NHTSA FARS data for multiple years
#'
#' Reads table data from one or more Fatality Analysis Reporting System (FARS) data files
#' containing observations from the specified year(s) and returns month and year for each observation.
#'
#' Will warn if any of the specified years is invalid.
#'
#' Year value is taken from the parameter not from the data file.
#'
#' @param years A vector of the years of observations to read.
#'
#' @return A list of tibbles, one tibble for each year specified in the parameter, contraining columns: MONTH, year.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @importFrom dplyr %>% mutate select
#'
#' @export
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

#' NHTSA FARS summary: number of fatal injuries per month for the specified years
#'
#' Reads Fatality Analysis Reporting System (FARS) data files and builds summary table containing
#' number of fatal injuries suffered in motor vehicle traffic crashes for each month of the specified years.
#'
#' @param years A vector of the years of observations to read.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draw map of NHTSA FARS accidents
#'
#' Reads Fatality Analysis Reporting System (FARS) data files and plots maps of accidents for the specified state and year
#' with the graphics package.
#'
#' Will stop with error if given state number is invalid. Will warn if no data are available for a given year.
#'
#' @param state.num USA state number, numeric.
#' @param year Year of ovservations, numeric.
#'
#' @return No return value (NULL).
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2015)
#' }
#'
#' @importFrom dplyr %>% filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("Invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("No accidents to plot")
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
