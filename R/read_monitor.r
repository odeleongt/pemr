#' Single function to call when reading monitor files.
#'
#' @description
#' Typical output from personal environmental monitors will be plain text files
#' containing either raw data in a readily usable format (e.g. PATS+, UPAS) or
#' raw data in binary format which needs to be pre-processed using the monitor
#' software (e.g. ECM).
#'
#' `read_monitor()` will read data from a single file and return a data frame
#' with all the columns contained in the file.
#'
#' @details
#'
#' Usually you will be interested in reading data form multiple files, this
#' can be easily achieved by listing in a data frame all files and types you
#' want to read and iterating over it with \code{\link{purrr::map2}} as shown
#' in the examples below.
#'
#' @param file Path to the file containing monitor data.
#' @param type Type of monitor with which the data was recorded.
#'   Currently there are five valid types:
#'
#'   * "patsp": data from PATS+ monitors.
#'   * "upas": data from UPAS monitors.
#'   * "ecm": data from ECM.
#'   * "ecm-raw": raw files from ecm will usually be located among the files,
#'   this will return an empty data frame.
#'   * "sums": reading data from SUMs is supported, but data is not used.
#'
#'   Any other option will be coerced to "unknown" and return an empty data
#'   frame.
#' @param ... Additional arguments passed to \code{\link{readr::read_csv}}
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
#'   data = map2(files_col, types_col, read_monitor)
#' )
#' }
#'
read_monitor <- function(file, type, ...){
  if(!is.na(file)){
    # Error checking
    if(!file.exists(file)) stop("File not found\n", file, "\n\n", call. = FALSE)
  }

  # Pick function
  read_em_ <- switch(
    type,
    "ecm-raw" = read_em_ecm_raw,
    "ecm" = read_em_ecm,
    "patsp" = read_em_patsp,
    "upas" = read_em_upas,
    "sums" = read_em_sums,
    read_em_unknown
  )

  return( suppressMessages( suppressWarnings( read_em_(file, ...) ) ) )
}




#------------------------------------------------------------------------------*
# Define helper funtions ----
#------------------------------------------------------------------------------*

# Get start line
get_start <- function(file, pattern, n){
  start <- readLines(con = file, n = n) %>%
    grep(pattern, .) %>%
    first()
  start - 1
}

# Set NA row
na_row <- function(column_spec){
  column_spec$cols %>%
    names() %>%
    set_names() %>%
    lapply( function(...) NA ) %>%
    as_data_frame()
}




#------------------------------------------------------------------------------*
# Define column types ----
#------------------------------------------------------------------------------*

# UPAS
upas_columns <- readr::cols(
  timestr = readr::col_datetime(format ="%y%m%d%H%M%S"),
  volflow = readr::col_double(),
  sampledVol = readr::col_double(),
  bme_temp = readr::col_double(),
  bme_press = readr::col_double(),
  bme_rh = readr::col_double(),
  bmp_temp = readr::col_double(),
  bmp_press = readr::col_double(),
  atmoRho = readr::col_double(),
  dpSDPu25 = readr::col_double(),
  tempSDPu25 = readr::col_double(),
  bVolt = readr::col_integer(),
  bFuel = readr::col_integer(),
  gpslatitude = readr::col_double(),
  gpslongitude = readr::col_double(),
  gpsUTCDate = readr::col_character(),
  gpsUTCTime = readr::col_character(),
  gpssatellites = readr::col_integer(),
  gpsaltitude = readr::col_double()
)

# PATS+
pats_columns <- readr::cols(
  dateTime = readr::col_datetime(format = ""),
  V_power = readr::col_double(),
  degC_sys = readr::col_double(),
  degC_air = readr::col_double(),
  `%RH_air` = readr::col_double(),
  degC_CO = readr::col_integer(),
  CO_PPM = readr::col_integer(),
  status = readr::col_integer(),
  ref_sigDel = readr::col_integer(),
  low20avg = readr::col_integer(),
  high320avg = readr::col_integer(),
  motion = readr::col_integer(),
  CO_mV = readr::col_integer(),
  X14 = readr::col_integer(),
  iButton_Temp = readr::col_double(),
  PM_Estimate = readr::col_character()
)


# ECM
ecm_columns <- readr::cols(
  Date = readr::col_character(),
  Time = readr::col_time(format = ""),
  `RH-Corrected Nephelometer` = readr::col_double(),
  `RH-Corrected Nephelometer HR` = readr::col_character(),
  Temp = readr::col_double(),
  RH = readr::col_double(),
  Battery = readr::col_double(),
  `Inlet Press` = readr::col_double(),
  `Orifice Press` = readr::col_double(),
  Flow = readr::col_double(),
  `X-axis` = readr::col_double(),
  `Y-axis` = readr::col_double(),
  `Z-axis` = readr::col_double(),
  `Vector Sum Composite` = readr::col_double(),
  ShutDownReason = readr::col_character(),
  `Wearing Compliance` = readr::col_character(),
  `ValidityWearingCompliance validation` = readr::col_character()
)


# SUMS
sums_columns <- readr::cols(
  `Date/Time` = readr::col_datetime(format = "%d/%m/%y %I:%M:%S %p"),
  Unit = readr::col_character(),
  Value = readr::col_double()
)




#------------------------------------------------------------------------------*
# Define functions for instrument types ----
#------------------------------------------------------------------------------*

# UPAS files
read_em_upas <- function(file, skip = NULL, ...){
  if(is.na(file)){
    return( na_row(column_spec = upas_columns) )
  } else {
    # Get data start
    if(is.null(skip)){
      skip <- get_start(file, pattern = "^timestr", n = 10)
    }

    # Read data
    readr::read_csv(file, skip = skip, col_types = upas_columns, ...) %>%
      mutate(
        date = date(timestr)
      ) %>%
      select(date, date_time = timestr, everything())
  }
}


# PATS+ files
read_em_patsp <- function(file, ...){
  if(is.na(file)){
    return( na_row(column_spec = pats_columns) )
  } else {
    # Get data start
    skip <- get_start(file, pattern = "^dateTime", n = 40)

    # Read data
    readr::read_csv(file, skip = skip, col_types = pats_columns, ...) %>%
      mutate(
        date = date(dateTime)
      ) %>%
      select(
        date, date_time = dateTime, everything(),
        -PM_Estimate, PM_Estimate = iButton_Temp
      )
  }
}



# ECM processed files
read_em_ecm <- function(file, skip = 24, ...){
  if(is.na(file)){
    return( na_row(column_spec = ecm_columns) )
  } else {
    # Get data start
    skip <- get_start(file, pattern = "^Date,", n = 40)

    # Column names
    column_names <- scan(
      file = file, quiet = TRUE,
      what = "character", sep = ",", skip = skip, nlines = 1
    )

    # Read contents
    readr::read_csv(
      file, skip = skip + 2, col_names = column_names, col_types = ecm_columns,
      ...
    ) %>%
      mutate(
        date1 = mdy(Date),
        date2 = dmy(Date),
        date3 = ymd(Date),
        date = if_else(
          condition = rep(all(is.na(date1)), n()),
          true = date2,
          false = date1
        ),
        date = if_else(
          condition = rep(all(is.na(date)), n()),
          true = date3,
          false = date
        ),
        date_time = ymd_hms(paste(date, Time))
      ) %>%
      select(date = date, date_time, everything(), -matches("date[0-9]"))
  }
}


# ECM raw files
read_em_ecm_raw <- function(file, ...){
  warning("Attempted to read a raw ECM file. Ignored.")
  data_frame()
}


# SUMS files
read_em_sums <- function(file, skip = 19, ...){
  if(is.na(file)){
    return( na_row(column_spec = sums_columns) )
  } else {
    # Get data start
    skip <- get_start(file, pattern = "^Date/Time,", n = 25)

    # Column names
    column_names <- scan(
      file = file, quiet = TRUE,
      what = "character", sep = ",", skip = skip, nlines = 1
    )

    # Read contents
    readr::read_csv(
      file, skip = skip + 1, col_names = column_names, col_types = sums_columns,
      ...
    ) %>%
      mutate(
        date = date(`Date/Time`)
      ) %>%
      select(date, date_time = `Date/Time`, everything())
  }
}


# Unknown monitors
read_em_unknown <- function(file, ...){
  warning(
    "Attempted to read a file from an unknown monitor. Ignored."
  )
  data_frame()
}
