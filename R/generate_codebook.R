# R/internal_helpers.R

#' Calculate Numeric Statistics
#'
#' Helper function to calculate statistics for numeric variables.
#'
#' @param var A numeric vector.
#'
#' @return A formatted string with descriptive statistics, including minimum, average, median, maximum, and standard deviation.
#'
#' @keywords internal
get_numeric_stats <- function(var) {
  if (all(is.na(var))) {
    return("All values are NA")
  }
  paste(
    "Min:", sprintf("%.1f", base::min(var, na.rm = TRUE)), "<br>",
    "Avg:", sprintf("%.1f", base::mean(var, na.rm = TRUE)), "<br>",
    "Median:", sprintf("%.1f", stats::median(var, na.rm = TRUE)), "<br>",
    "Max:", sprintf("%.1f", base::max(var, na.rm = TRUE)), "<br>",
    "SD:", sprintf("%.1f", stats::sd(var, na.rm = TRUE))
  )
}

#' Calculate Categorical Statistics
#'
#' Helper function to calculate top statistics for categorical variables.
#'
#' @param var A factor or character vector representing categorical data.
#' @param top_n An integer specifying the number of top categories to display. Default is 5.
#'
#' @return A formatted string listing the top categories with their counts and percentages, or all categories if the number of levels is less than or equal to `top_n`.
#'
#' @keywords internal
get_categorical_stats <- function(var, top_n = 5) {
  if (all(is.na(var))) {
    return("All values are NA")
  }

  # Generate table of counts
  level_counts <- sort(table(var, useNA = "no"), decreasing = TRUE)

  # Exclude categories with zero counts
  level_counts <- level_counts[level_counts > 0]

  total_counts <- sum(level_counts)
  num_levels <- length(level_counts)
  top_levels <- names(level_counts)

  if (num_levels > top_n) {
    top_levels <- top_levels[1:top_n]
    level_counts <- level_counts[1:top_n]
    label <- paste0("Top ", top_n, " Categories:<br>")
  } else {
    label <- "All Categories:<br>"
  }

  # Format percentages with one decimal place
  percentages <- sprintf("%.1f", (level_counts / total_counts) * 100)

  # Concatenate the statistics with controlled spacing
  top_stats <- paste(
    top_levels,
    "=",
    level_counts,
    paste0("(", percentages, "%)"),
    sep = " ",
    collapse = "<br>"
  )

  paste0(label, top_stats)
}

#' Calculate Date Statistics
#'
#' Helper function to calculate statistics for Date variables.
#'
#' @param var A Date vector.
#'
#' @return A formatted string with the minimum and maximum dates.
#'
#' @keywords internal
get_date_stats <- function(var) {
  if (all(is.na(var))) {
    return("All values are NA")
  }
  min_date <- min(var, na.rm = TRUE)
  max_date <- max(var, na.rm = TRUE)
  paste(
    "Min:", as.character(min_date), "<br>",
    "Max:", as.character(max_date), "<br>"
  )
}

#' Calculate Missing Value Information
#'
#' Helper function to calculate missing value information.
#'
#' @param var A vector of any type.
#'
#' @return A list containing the number and percentage of missing values.
#'
#' @keywords internal
get_missing_info <- function(var) {
  total_values <- length(var)
  num_missing <- sum(is.na(var))
  percent_missing <- round((num_missing / total_values) * 100, 1)
  list(num_missing = num_missing, percent_missing = percent_missing)
}

#' Calculate Logical Statistics
#'
#' Helper function to calculate statistics for logical variables.
#'
#' @param var A logical vector.
#'
#' @return A formatted string with counts and percentages of TRUE and FALSE values.
#'
#' @keywords internal
get_logical_stats <- function(var) {
  if (all(is.na(var))) {
    return("All values are NA")
  }

  true_count <- sum(var, na.rm = TRUE)
  false_count <- sum(!var & !is.na(var))
  total_count <- true_count + false_count

  # Format percentages with one decimal place and no space before '%'
  true_percent <- if (total_count > 0) sprintf("%.1f", (true_count / total_count) * 100) else "0.0"
  false_percent <- if (total_count > 0) sprintf("%.1f", (false_count / total_count) * 100) else "0.0"

  # Concatenate the statistics with controlled spacing and formatting
  paste0(
    "TRUE: ", true_count, " (", true_percent, "%)<br>",
    "FALSE: ", false_count, " (", false_percent, "%)"
  )
}

#' Generate a Codebook for a Data Frame
#'
#' This function generates a codebook summarizing the variables in a data frame, including
#' statistics for numeric, categorical, logical, and date variables. The codebook can be output
#' in either `reactable` or `kable` format.
#'
#' @param df A data frame for which to generate the codebook.
#' @param var_descriptions An optional named list providing descriptions for variables.
#' @param hide_statistics An optional character vector of column names for which statistics should be hidden. This is particularly useful for personally identifiable information.
#' @param top_n An integer specifying the number of top categories to display for categorical variables. Default is 5.
#'
#' @return A formatted codebook as a `reactable` or `kable` object.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   age = c(25, 30, 22, 40, NA),
#'   gender = factor(c("Male", "Female", "Female", "Male", NA)),
#'   start_date = as.Date(c("2020-01-01", "2020-02-15", NA, NA, "2020-04-20")),
#'   active = c(TRUE, FALSE, TRUE, NA, FALSE)
#' )
#' var_desc <- list(
#'   age = "Age of the client",
#'   gender = "Gender of the client",
#'   start_date = "Date of supervision start date",
#'   active = "Active status of the client"
#' )
#' generate_codebook(df, var_descriptions = var_desc)
#' }
#'
#' @importFrom dplyr bind_rows rename_with %>% everything
#' @importFrom stats median sd
#' @importFrom reactable reactable colDef
#' @importFrom htmltools HTML
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom progress progress_bar
#' @export
generate_codebook <- function(df, var_descriptions = NULL, hide_statistics = NULL, top_n = 5) {

  ### 1. Input Validation ###

  # a. Validate 'df'
  if (!is.data.frame(df)) {
    stop("Error: The input 'df' must be a data frame.")
  }

  if (nrow(df) == 0) {
    stop("Error: The data frame 'df' is empty. Please provide a data frame with at least one row and one column.")
  }

  # Check for list-columns or nested data frames
  list_cols <- sapply(df, is.list)
  if (any(list_cols)) {
    stop("Error: The data frame 'df' contains list-columns, which are not supported.")
  }

  # b. Check for duplicate column names in 'df'
  if (any(duplicated(names(df)))) {
    duplicated_cols <- names(df)[duplicated(names(df))]
    stop(paste("Error: The data frame 'df' contains duplicate column names:",
               paste(unique(duplicated_cols), collapse = ", "),
               ". Please ensure all column names are unique."))
  }

  # c. Validate 'var_descriptions'
  if (!is.null(var_descriptions)) {
    if (!is.list(var_descriptions)) {
      stop("Error: The input 'var_descriptions' must be a named list.")
    }
    if (is.null(names(var_descriptions)) || any(names(var_descriptions) == "")) {
      stop("Error: All elements in 'var_descriptions' must be named, corresponding to column names in 'df'.")
    }
    invalid_desc_names <- setdiff(names(var_descriptions), names(df))
    if (length(invalid_desc_names) > 0) {
      stop(paste("Error: The following names in 'var_descriptions' do not match any columns in 'df':",
                 paste(invalid_desc_names, collapse = ", ")))
    }
    # Check for non-character or NULL descriptions
    invalid_desc <- sapply(var_descriptions, function(desc) !is.character(desc) || length(desc) != 1 || is.na(desc))
    if (any(invalid_desc)) {
      stop("Error: All descriptions in 'var_descriptions' must be single, non-empty character strings.")
    }
  }

  # d. Validate 'hide_statistics'
  if (!is.null(hide_statistics)) {
    if (!is.character(hide_statistics)) {
      stop("Error: The input 'hide_statistics' must be a character vector of column names.")
    }
    invalid_hide <- setdiff(hide_statistics, names(df))
    if (length(invalid_hide) > 0) {
      stop(paste("Error: The following names in 'hide_statistics' do not match any columns in 'df':",
                 paste(invalid_hide, collapse = ", ")))
    }
  }

  ### 2. Error Handling for 'top_n' ###

  # a. Capture the unevaluated expression passed to 'top_n'
  top_n_expr <- substitute(top_n)

  # b. Evaluate 'top_n' within the parent environment
  top_n_val <- tryCatch(
    eval(top_n_expr, envir = parent.frame()),
    error = function(e) {
      stop("Error: 'top_n' must be a single positive integer. It appears you have passed an undefined variable or a non-numeric value. Please provide a valid numeric value, e.g., top_n = 3.")
    }
  )

  # c. Validate the evaluated 'top_n' value
  if (!is.numeric(top_n_val) || length(top_n_val) != 1 || top_n_val < 1 || top_n_val != as.integer(top_n_val)) {
    stop("Error: 'top_n' must be a single positive integer. Please provide a valid numeric value, e.g., top_n = 3.")
  }

  ### 3. Progress Bar ###

  if (requireNamespace("progress", quietly = TRUE)) {
    use_progress <- TRUE
    pb <- progress::progress_bar$new(
      format = "  Processing [:bar] :percent eta: :eta",
      total = ncol(df), clear = FALSE, width = 60
    )
  } else {
    use_progress <- FALSE
    message("Package 'progress' not found. Progress bar will be disabled.")
  }

  ### 4. Process Each Column to Build the Codebook ###

  codebook <- lapply(names(df), function(colname) {
    var <- df[[colname]]

    # Update progress bar
    if (use_progress) {
      pb$tick()
    }

    # a. Variable Description
    var_desc <- if (!is.null(var_descriptions) && !is.null(var_descriptions[[colname]])) {
      var_descriptions[[colname]]
    } else {
      "Description needed."
    }

    # b. Variable Type
    var_type <- class(var)[1]

    # c. Number of Unique Values
    num_unique <- length(unique(var[!is.na(var)]))

    # d. Missing Value Information
    missing_info <- get_missing_info(var)

    # e. Determine Statistics Based on Variable Type
    stats <- if (is.numeric(var)) {
      get_numeric_stats(var)
    } else if (is.factor(var) || is.character(var)) {
      get_categorical_stats(var, top_n = top_n_val)
    } else if (inherits(var, "Date")) {
      get_date_stats(var)
    } else if (is.logical(var)) {
      get_logical_stats(var)
    } else {
      "Unsupported type"
    }

    # f. Hide Statistics if Specified
    if (!is.null(hide_statistics) && colname %in% hide_statistics) {
      stats <- "Hidden"
      message(paste("Statistics for variable '", colname, "' are hidden.", sep = ""))
    } else if (stats == "Unsupported type") {
      message(paste("Variable '", colname, "' has an unsupported type and its statistics are not included.", sep = ""))
    }

    # g. Compile into a Data Frame Row
    data.frame(
      variable_name = colname,
      variable_description = var_desc,
      variable_type = var_type,
      number_of_unique_values = num_unique,
      percentage_missing = paste0(sprintf("%.1f", missing_info$percent_missing), "%"),
      statistics = stats,
      stringsAsFactors = FALSE
    )
  })

  codebook <- dplyr::bind_rows(codebook)

  ### 5. Formatting Column Names ###

  codebook <- codebook %>%
    dplyr::rename_with(~ tools::toTitleCase(gsub("_", " ", .)), everything())

  ### 6. Handle HTML in Statistics ###

  codebook$Statistics <- lapply(codebook$Statistics, htmltools::HTML)

  return(
    knitr::kable(codebook, format = "html", escape = FALSE) %>%
      kableExtra::kable_styling("striped", full_width = FALSE)
  )

}

