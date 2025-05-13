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
#' This function generates a detailed codebook for a given data frame, summarizing
#' information about each variable, such as its type, the number of unique values,
#' percentage of missing values, and descriptive statistics.
#'
#' @param df A data frame for which the codebook is to be generated.
#' @param hide_statistics An optional character vector specifying variable names
#'   for which statistics should be hidden. Default is `NULL`.
#' @param top_n An integer specifying the number of top categories to display
#'   for categorical variables. Default is 5.
#' @param extra_vars An optional data frame with additional metadata about
#'   variables in `df`. This metadata will be merged with the generated codebook.
#'   Default is `NULL`.
#' @param extra_key A character string specifying the column name in `extra_vars`
#'   that corresponds to the variable names in `df`. If `NULL`, the function assumes
#'   that the first column of `extra_vars` contains the variable names. Default is `NULL`.
#'
#' @return An HTML-formatted table summarizing the codebook for the data frame.
#'
#' @details
#' - Numeric variables include statistics like minimum, average, median, maximum,
#'   and standard deviation.
#' - Categorical variables include the top categories and their counts and percentages.
#' - Date variables include the minimum and maximum dates.
#' - Logical variables summarize the counts and percentages of `TRUE` and `FALSE` values.
#'
#' The function also handles additional metadata by merging it with the generated
#' codebook if provided through `extra_vars`.
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   Age = c(25, 30, NA, 40),
#'   JoinedDate = as.Date(c("2020-01-01", "2021-06-15", NA, "2019-11-30")),
#'   Active = c(TRUE, FALSE, TRUE, NA)
#' )
#'
#' # Generate a codebook
#' databook(df)
#'
#' # Generate a codebook with additional metadata
#' extra_vars <- data.frame(
#'   `Variable Name` = c("Age"),
#'   Description = c("Participant age")
#' )
#' databook(df, extra_vars = extra_vars)
#'
#' @importFrom dplyr %>% everything bind_rows left_join rename_with
#' @importFrom tools toTitleCase
#' @importFrom htmltools HTML
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @export
databook <- function(df,
                     hide_statistics = NULL,
                     top_n = 5,
                     extra_vars = NULL,
                     extra_key = NULL) {
  ### 1. Input Validation ###
  if (!is.data.frame(df)) stop("Error: The input 'df' must be a data frame.")
  if (nrow(df) == 0) stop("Error: The data frame 'df' is empty.")
  if (any(sapply(df, is.list))) stop("Error: The data frame contains list-columns, which are not supported.")
  if (anyDuplicated(names(df))) stop("Error: The data frame contains duplicate column names.")

  # Validate `top_n`
  if (!is.numeric(top_n) || length(top_n) != 1 || top_n <= 0 || top_n %% 1 != 0) {
    stop("Error: 'top_n' must be a single positive integer.")
  }

  # Validate `hide_statistics`
  if (!is.null(hide_statistics)) {
    if (!is.character(hide_statistics)) {
      stop("Error: 'hide_statistics' must be a character vector.")
    }
    invalid_names <- setdiff(hide_statistics, names(df))
    if (length(invalid_names) > 0) {
      stop(paste(
        "Error: The following names in 'hide_statistics' do not match any column names in the data frame:",
        paste(invalid_names, collapse = ", ")
      ))
    }
  }

  ### 2. Process Each Column ###
  codebook <- lapply(names(df), function(colname) {
    var <- df[[colname]]
    var_type <- class(var)[1]
    num_unique <- length(unique(var[!is.na(var)]))
    missing_info <- get_missing_info(var)

    stats <- if (is.numeric(var)) {
      get_numeric_stats(var)
    } else if (is.factor(var) || is.character(var)) {
      get_categorical_stats(var, top_n = top_n)
    } else if (inherits(var, "Date")) {
      get_date_stats(var)
    } else if (is.logical(var)) {
      get_logical_stats(var)
    } else {
      message(paste("Variable", colname, "has an unsupported type:", var_type))
      "Unsupported type"
    }

    if (!is.null(hide_statistics) && colname %in% hide_statistics) {
      stats <- "Hidden"
    }

    data.frame(
      `Variable Name` = colname,
      `Variable Type` = var_type,
      `Number of Unique Values` = num_unique,
      `Percentage Missing` = paste0(sprintf("%.1f", missing_info$percent_missing), "%"),
      `Statistics` = stats,
      stringsAsFactors = FALSE
    )
  }) %>% dplyr::bind_rows()

  ### 3. Standardize Column Names ###
  # Ensure `Variable Name` exists in `codebook`
  names(codebook) <- gsub("\\.", " ", names(codebook))

  ### 4. Merge with Extra Variables ###
  if (!is.null(extra_vars)) {
    if (!is.data.frame(extra_vars)) {
      stop("Error: 'extra_vars' must be a data frame.")
    }

    # Check for empty data frame
    if (nrow(extra_vars) == 0) {
      stop("Error: 'extra_vars' is an empty data frame.")
    }

    if (is.null(extra_key)) {
      # Assume first column contains variable names if `extra_key` is NULL
      extra_key <- names(extra_vars)[1]
      warning(paste(
        "Warning: 'extra_key' is not specified.",
        "Using the first column of 'extra_vars' as the variable name key:", extra_key
      ))
    }

    if (!(extra_key %in% names(extra_vars))) {
      stop(paste("Error: 'extra_key' must be a valid column name in 'extra_vars'.",
                 "The specified key", extra_key, "is not found."))
    }

    if (!"Variable Name" %in% names(codebook)) {
      stop("Error: The codebook must contain a 'Variable Name' column to merge with 'extra_vars'.")
    }

    # Ensure extra_key has no duplicate values
    if (any(duplicated(extra_vars[[extra_key]]))) {
      stop(paste("Error: The column specified by 'extra_key' (", extra_key, ") in 'extra_vars' contains duplicate values."))
    }

    # Rename join column in extra_vars to match codebook
    names(extra_vars)[names(extra_vars) == extra_key] <- "Variable Name"

    # Perform the join
    codebook <- dplyr::left_join(codebook, extra_vars, by = "Variable Name")
  }

  ### 5. Format Output ###
  codebook <- codebook %>%
    dplyr::rename_with(~ tools::toTitleCase(gsub("_", " ", .)), everything())
  codebook$Statistics <- lapply(codebook$Statistics, htmltools::HTML)

  # knitr::kable(codebook, format = "html", escape = FALSE) %>%
  #   kableExtra::kable_styling("striped", full_width = FALSE)
  return(codebook)
}
