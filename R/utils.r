#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom tibble data_frame
#' @export
tibble::data_frame

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom tidyr unnest
#' @export
tidyr::unnest

#' @importFrom purrr map2
#' @export
purrr::map2





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
