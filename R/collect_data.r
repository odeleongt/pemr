#' Single function to call when extracting variables from each monitor.
#'
#' @description
#' When following the recommended use of \code{\link{read_monitor}}
#' (data frame listing files and types, iterate with `purrr::map2` to read),
#' the resulting object will include a list column containing onw data frame
#' with the data for each file (nested data).
#'
#' `collect_data` processes the nested data and produces a data frame with all
#' observations of `date_time`, `concentration` and `flow` from all the read
#' files.
#'
#' Additional columns should be included ad a key to identify the data from each
#' instrument in the resulting data frame.
#'
#' @details
#'
#' Since some monitors can potentially produce files with record date/times
#' using different date formats (e.g. d/m/y, m/d/y, y/m/d) this function
#' tries to guess the correct format but can sometimes miss.
#' It is recommended to provide a reference date for each file so you can check
#' whether date/times were correctly interpreded.
#'
#' There is no clear column in the UPAS files to extract the concentration data,
#' for now this functions returns `NA` for this concentration.
#'
#' @param .data Dataframe containing a nested column with the data from each
#'   file. Ussually a product of iterating `read_monitor` using `purrr::map2`.
#' @param type Type of monitor with which the data was recorded.
#' @param data_col Column containing the nested data.
#' @param ... Additional columns present in `.data` to preserve in the output.
#'   Useful to define the key to identify each data set.
#' @return A data frame with `type`, specified key columns and collected
#' measures for `date_time`, `concentration` and `flow`.
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
#' ) %>%
#' collect_data(
#'   type = types_col, data_col = data,
#'   files_col
#' )
#' }
collect_data <- function(.data, type, data_col, ...){
  # Work with tidy evaluation
  quo_type <- enquo(type)
  quo_data_col <- enquo(data_col)
  vars <- quos(...)

  #----------------------------------------------------------------------------*
  # Do for each type of monitor ----
  #----------------------------------------------------------------------------*

  monitor_types <- .data %>% pull(!!quo_type) %>% unique

  if(length(monitor_types) > 1){
    # Get data for each monitor type
    monitor_data <- .data %>%
      split(.[, rlang::quo_name(quo_type)]) %>%
      map(
        collect_data, !!quo_type, !!quo_data_col, !!!vars
      ) %>%
      bind_rows()
  } else {
    # Filter monitor types
    type <- ifelse(
      test = monitor_types %in% c("ecm", "ecm-raw", "patsp", "upas"),
      yes = monitor_types,
      no = "unknown"
    )

    # Pick function
    data_em_ <- switch(
      type,
      "ecm-raw" = data_em_ecm_raw,
      "ecm" = data_em_ecm,
      "patsp" = data_em_patsp,
      "upas" = data_em_upas,
      "sums" = data_em_na,
      "unknown" = data_em_na
    )
    monitor_data <- .data %>%
      unnest(!!quo_data_col) %>%
      data_em_(.data = , type = quo_type, vars = vars)
  }

  return(monitor_data)
}




#------------------------------------------------------------------------------*
# Extract variables of interest for each monitor ----
#------------------------------------------------------------------------------*

# ecm-raw
data_em_ecm_raw <- function(.data, type, vars){
  .data %>%
    # Force missing variables
    mutate(
      date_time = as.POSIXct(NA),
      concentration = NA_real_,
      flow = NA_real_
    ) %>%
    # Keep monitor type, any requested variables, and data variables
    select(
      !!!type, !!!vars,
      date_time, concentration, flow
    )
}

# unknown / not applicable
data_em_na <- function(.data, type, vars){
  .data %>%
    # Force missing variables
    mutate(
      date_time = as.POSIXct(NA),
      concentration = NA_real_,
      flow = NA_real_
    ) %>%
    # Keep monitor type, any requested variables, and data variables
    select(
      !!!type, !!!vars,
      date_time, concentration, flow
    )
}

# ecm-processed
data_em_ecm <- function(.data, type, vars){
  .data %>%
    # Keep monitor type, any requested variables, and data variables
    select(
      !!!type, !!!vars,
      date_time, concentration = `RH-Corrected Nephelometer`, flow = Flow
    )
}

# upas
data_em_upas <- function(.data, type, vars){
  .data %>%
    # Force missing variables
    mutate(
      concentration = NA
    ) %>%
    # Keep monitor type, any requested variables, and data variables
    select(
      !!!type, !!!vars,
      date_time, concentration = atmoRho, flow = volflow
    )
}

# pats+
data_em_patsp <- function(.data, type, vars){
  .data %>%
    # Force missing variables
    mutate(
      flow = NA_real_,
      PM_Estimate = as.numeric(PM_Estimate)
    ) %>%
    # Keep monitor type, any requested variables, and data variables
    select(
      !!!type, !!!vars,
      date_time, concentration = PM_Estimate, flow
    )
}
