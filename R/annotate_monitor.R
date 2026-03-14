#' Single function to call when reading monitor files.
#'
#' @description
#' Typical output from personal environmental monitors will be plain text files
#' containing either raw data in a readily usable format (e.g. PATS+, UPAS) or
#' raw data in binary format which needs to be pre-processed using the monitor
#' software (e.g. ECM).
#'
#' `annotate_monitor()` will read data from the header of a single file and
#' return a data frame with all the metadata contained in that header.
#'
#' @details
#'
#' Usually you will be interested in reading data form multiple files, this
#' can be easily achieved by listing in a data frame all files and types you
#' want to read and iterating over it with \code{\link{purrr::map2}} as shown
#' in the examples below. Use \code{annotate_monitor} in the same way to
#' read in the header metadata for each file, and then use
#'  \code{\link{tidyr::unnest}} to extract all the metadata for each file.
#'
#' @param file Path to the file containing monitor metadata.
#' @param type Type of monitor with which the data was recorded.
#'   Currently there are two valid types:
#'
#'   * "ecm" or "ecm-full": metadata from ECM.
#'
#'   Any other option will be coerced to "unknown" and return an empty data
#'   frame.
#' @param ... Ignored
#' @return A data frame with all columns from the monitor file.
#' @export
#' @examples
#' \dontrun{
#' # Load required packages
#' library(package = "pemr")
#'
#' # Define files and types. This can be any data frame, so composing data in a
#' # spreadsheet and reading it in works too
#' data_frame(
#'  files_col = c("path/to/file1.csv", "path/to/file2.csv"),
#'  types_col = c("ecm", "upas")
#' ) %>%
#' mutate(
#'   data = map2(files_col, types_col, read_monitor),
#'   header = map2(files_col, types_col, annotate_monitor)
#' ) %>%
#' unnest()
#' }
#'
annotate_monitor <- function(file, type, ...){
  if(!is.na(file)){
    # Error checking
    if(!file.exists(file)) stop("File not found\n", file, "\n\n", call. = FALSE)
  }

  # Pick function
  head_em_ <- switch(
    type,
    "ecm-full" = head_em_ecm,
    "ecm" = head_em_ecm,
    head_em_unknown
  )

  return( suppressMessages( suppressWarnings( head_em_(file, ...) ) ) )
}




#------------------------------------------------------------------------------*
# Read file headers ----
#------------------------------------------------------------------------------*


# Define header variables
header_ecm_vars <- c(
  device_id = "Device Serial #",
  filter_id = "Filter ID#",
  device_cycling = "System Times",
  date_time_hardware = "Date/Time Hardware",
  date_time_software = "Date/Time Software",
  inlet_aerosol_size = "Inlet Aerosol Size",
  laser_cycling = "Laser Cycling Variables",
  sensor_params = "Sensor",
  nephelometer_params = "Nephelometer",
  temperature_params = "Temperature",
  humidity_params = "Humidity",
  flow_params = "Flow",
  pressure_params_inlet = "Inlet Pressure",
  pressure_params_orifice = "Orifice Pressure",
  accelerometer_delay = "Accelerometer",
  battery_delay = "Battery",
  raw_file_name = "File Name",
  download_date = "Download Date",
  download_time = "Total Download Time (s)",
  ventilation = "Ventilation",
  participant_id = "Participant ID#",
  participant_wh = "Participant Weight (kg)"
)


# ECM file header
head_em_ecm <- function(file, ...){
  if(is.na(file)){
    return( na_row(column_spec = list(cols = header_ecm_vars)) )
  } else {
    # Get data start
    n <- get_start(file, pattern = "^Date,", n = 40)

    get_value <- purrr::possibly(function(x) x[2], otherwise = NA_character_)

    # Read header rows
    header <- scan(
      file = file, quiet = TRUE,
      what = "character", sep = "\n", nlines = n
    ) %>%
      # Split lines into variable value pairs
      gsub("^([^,:]+)[,:]+", "\\1;", .) %>%
      strsplit(split = ";") %>%
      purrr::map(~ data_frame(!!.x[1] := get_value(.x))) %>%
      dplyr::bind_cols() %>%
      dplyr::select(!!!header_ecm_vars)

    return(header)
  }
}


# Unknown monitors
head_em_unknown <- function(file, ...){
  warning(
    "Attempted to read a file from an unknown monitor. Ignored."
  )
  data_frame()
}
