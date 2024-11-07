#' Generate a Codebook
#'
#' This function creates a codebook for a given dataset.
#'
#' @param data A data frame for which to generate the codebook.
#' @return A data frame or list containing variable descriptions.
#' @importFrom dplyr slice
#' @export
generate_codebook <- function(data) {
  data |>
    slice(1:2)
}
