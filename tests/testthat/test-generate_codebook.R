# test-generate_codebook.R
library(testthat)
library(dplyr)
library(knitr)
library(kableExtra)
library(reactable)
library(htmltools)
library(xml2)
library(rvest)

# Define a sample data frame for testing
sample_df <- data.frame(
  numeric_var = c(10, 20, 30, NA, 50),
  factor_var = factor(c("A", "B", "A", "C", NA)),
  character_var = c("X", "Y", "X", "Z", "Y"),
  date_var = as.Date(c("2021-01-01", "2021-06-15", NA, "2021-12-31", "2021-03-10")),
  logical_var = c(TRUE, FALSE, TRUE, NA, FALSE),
  unsupported_var = I(list(1, 2, 3, 4, 5)), # List-column to test unsupported type
  stringsAsFactors = FALSE
)

# Define variable descriptions
var_desc <- list(
  numeric_var = "Numeric variable for testing",
  factor_var = "Factor variable with categories A, B, C",
  character_var = "Character variable with categories X, Y, Z",
  date_var = "Date variable for testing",
  logical_var = "Logical variable indicating TRUE or FALSE"
)

test_that("generate_codebook returns error for non-data.frame input", {
  expect_error(generate_codebook(list(a = 1, b = 2)), "must be a data frame")
})

test_that("generate_codebook returns error for empty data frame", {
  expect_error(generate_codebook(data.frame()), "is empty")
})

test_that("generate_codebook returns error for data frames with list-columns", {
  df <- data.frame(
    a = I(list(1, 2, 3)),
    b = c("x", "y", "z")
  )
  expect_error(generate_codebook(df), "contains list-columns")
})

test_that("generate_codebook returns error for duplicate column names", {
  df <- data.frame(
    a = 1:3,
    a = 4:6,
    check.names = FALSE  # Prevent R from renaming duplicate columns
  )
  expect_error(generate_codebook(df), "duplicate column names")
})

test_that("generate_codebook returns error for invalid var_descriptions", {
  df <- data.frame(
    a = 1:3,
    b = c("x", "y", "z")
  )

  # Non-named list
  expect_error(generate_codebook(df, var_descriptions = list("Description for a", "Description for b")),
               "must be named")

  # Names not matching
  expect_error(generate_codebook(df, var_descriptions = list(a = "Description for a", c = "Invalid")),
               "do not match")

  # Non-character descriptions
  expect_error(generate_codebook(df, var_descriptions = list(a = 123, b = "Valid")),
               "must be single, non-empty character strings")
})

test_that("generate_codebook returns error for invalid hide_statistics", {
  df <- data.frame(
    a = 1:3,
    b = c("x", "y", "z")
  )

  # Non-character vector
  expect_error(generate_codebook(df, hide_statistics = 123),
               "must be a character vector")

  # Names not matching
  expect_error(generate_codebook(df, hide_statistics = c("a", "c")),
               "do not match")
})






test_that("generate_codebook handles numeric variables correctly", {
  df <- data.frame(
    num = c(1, 2, 3, 4, 5, NA)
  )

  codebook <- generate_codebook(df)

  # Check if the statistics for num are correctly calculated
  expect_match(codebook, "Min: 1.0")
  expect_match(codebook, "Avg: 3.0")
  expect_match(codebook, "Median: 3.0")
  expect_match(codebook, "Max: 5.0")
  expect_match(codebook, "SD: 1.6")
})

test_that("generate_codebook handles categorical variables correctly", {
  df <- data.frame(
    cat = factor(c("apple", "banana", "apple", "cherry", "banana", "banana", NA))
  )

  codebook <- generate_codebook(df, top_n = 2)

  # Expect top 2 categories to be banana and apple with one decimal in percentage
  expect_match(codebook, "Top 2 Categories:")
  expect_match(codebook, "banana = 3 \\(50.0%\\)")
  expect_match(codebook, "apple = 2 \\(33.3%\\)")
})

test_that("generate_codebook handles date variables correctly", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-05-15", NA, "2020-12-31"))
  )

  codebook <- generate_codebook(df)

  expect_match(codebook, "Min: 2020-01-01")
  expect_match(codebook, "Max: 2020-12-31")
})

test_that("generate_codebook handles logical variables correctly", {
  df <- data.frame(
    flag = c(TRUE, FALSE, TRUE, TRUE, NA, FALSE)
  )

  codebook <- generate_codebook(df)

  # Check for correctly formatted TRUE statistics
  expect_match(codebook, "TRUE: 3 \\(60.0%\\)<br>FALSE: 2 \\(40.0%\\)")
})

test_that("generate_codebook hides statistics for specified variables", {
  df <- data.frame(
    num = c(1, 2, 3, 4, 5),
    cat = factor(c("A", "B", "A", "C", "B"))
  )

  codebook <- generate_codebook(df, hide_statistics = "num")

  expect_match(codebook, "num")
  expect_match(codebook, "Hidden")
  expect_match(codebook, "cat")
  expect_match(codebook, "All Categories:")
  expect_match(codebook, "A = 2 \\(40.0%\\)<br>B = 2 \\(40.0%\\)<br>C = 1 \\(20.0%\\)")
})

test_that("generate_codebook applies variable descriptions correctly", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = factor(c("X", "Y", "X"))
  )

  descriptions <- list(
    var1 = "First variable",
    var2 = "Second variable"
  )

  codebook <- generate_codebook(df, var_descriptions = descriptions)

  expect_match(codebook, "First variable")
  expect_match(codebook, "Second variable")
})

test_that("generate_codebook handles all NA variables", {
  df <- data.frame(
    all_na_numeric = c(NA, NA, NA),
    all_na_factor = factor(c(NA, NA, NA)),
    all_na_character = c(NA, NA, NA),
    all_na_date = as.Date(c(NA, NA, NA)),
    all_na_logical = c(NA, NA, NA)
  )

  codebook <- generate_codebook(df)

  expect_match(codebook, "All values are NA", fixed = TRUE, all = TRUE)
})

test_that("generate_codebook returns error for invalid top_n", {
  df <- data.frame(
    a = 1:3
  )

  # Non-numeric top_n
  expect_error(generate_codebook(df, top_n = "five"),
               "'top_n' must be a single positive integer")

  # Negative top_n
  expect_error(generate_codebook(df, top_n = -3),
               "'top_n' must be a single positive integer")

  # Non-integer top_n
  expect_error(generate_codebook(df, top_n = 2.5),
               "'top_n' must be a single positive integer")

  # Undefined variable
  expect_error(generate_codebook(df, top_n = undefined_var),
               "'top_n' must be a single positive integer")
})


test_that("generate_codebook handles unsupported variable types", {
  df <- data.frame(
    a = 1:3,
    b = I(list(1, 2, 3))
  )

  # Since 'b' is a list-column and should cause an error earlier, test with a different unsupported type
  # For example, a complex number
  df <- data.frame(
    a = 1:3,
    b = c(1+1i, 2+2i, 3+3i)
  )

  codebook <- generate_codebook(df)

  expect_match(codebook, "Unsupported type")
})

test_that("generate_codebook processes empty columns correctly", {
  df <- data.frame(
    empty_numeric = numeric(0),
    empty_factor = factor(character()),
    empty_character = character(),
    empty_date = as.Date(character()),
    empty_logical = logical()
  )

  # Since the data frame has 0 rows, it should raise an error
  expect_error(generate_codebook(df), "is empty")
})

test_that("generate_codebook includes HTML formatting in statistics", {
  df <- data.frame(
    num = c(1, 2, 3, 4, 5),
    cat = factor(c("A", "B", "A", "C", "B"))
  )

  codebook <- generate_codebook(df)

  # Check for HTML line breaks
  expect_match(codebook, "<br>", fixed = TRUE)

  # Check for HTML tags from kable and kableExtra
  expect_match(codebook, "<table", fixed = TRUE)
  expect_match(codebook, "<thead", fixed = TRUE)
  expect_match(codebook, "<tbody", fixed = TRUE)
})

test_that("generate_codebook handles large data frames efficiently", {
  # Create a large data frame
  large_df <- data.frame(
    num = rnorm(10000),
    cat = sample(LETTERS, 10000, replace = TRUE),
    date = sample(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2020-12-31"), by = "day"), 10000, replace = TRUE),
    logical = sample(c(TRUE, FALSE, NA), 10000, replace = TRUE)
  )

  # Expect no errors and a valid codebook
  expect_silent(codebook <- generate_codebook(large_df))
  expect_true(nchar(as.character(codebook)) > 0)
})

test_that("generate_codebook handles NA values correctly in logical variables", {
  df <- data.frame(
    logical_var = c(TRUE, FALSE, NA, TRUE, NA)
  )

  codebook <- generate_codebook(df)

  # TRUE: 2 (66.7%)
  # FALSE: 1 (33.3%)
  expect_match(codebook, "TRUE: 2 \\(66.7%\\)")
  expect_match(codebook, "FALSE: 1 \\(33.3%\\)")
})

test_that("generate_codebook does not include statistics for unsupported types", {
  df <- data.frame(
    a = 1:3,
    b = complex(real = 1:3, imaginary = 4:6)
  )

  # Since 'b' is complex, which is unsupported, check the message and that 'Unsupported type' is present
  expect_message(codebook <- generate_codebook(df), "has an unsupported type")
  expect_match(codebook, "Unsupported type")
})

test_that("generate_codebook handles progress bar gracefully when 'progress' package is unavailable", {
  # Mock the 'requireNamespace' function to return FALSE for 'progress'
  mock_requireNamespace <- function(package, quietly = TRUE) {
    if (package == "progress") {
      return(FALSE)
    } else {
      base::requireNamespace(package, quietly = quietly)
    }
  }

  # Stub 'requireNamespace' in 'generate_codebook' to use the mock function
  stub(generate_codebook, 'requireNamespace', mock_requireNamespace)

  # Create a mock data frame
  df <- data.frame(
    a = 1:5,
    b = letters[1:5]
  )

  # Expect a message about the progress bar being disabled
  expect_message(
    generate_codebook(df),
    "Package 'progress' not found. Progress bar will be disabled."
  )
})

test_that("generate_codebook handles variables with single unique value", {
  df <- data.frame(
    single_num = rep(5, 5),
    single_cat = factor(rep("A", 5)),
    single_char = rep("X", 5),
    single_date = rep(as.Date("2021-01-01"), 5),
    single_logical = rep(TRUE, 5)
  )

  codebook <- generate_codebook(df)

  # # Check number of unique values
  expect_match(codebook, "1")

  # Check statistics for single numeric value
  expect_match(codebook, "Min: 5.0")
  expect_match(codebook, "Avg: 5.0")
  expect_match(codebook, "Median: 5.0")
  expect_match(codebook, "Max: 5.0")
  expect_match(codebook, "SD: 0.0")

  # Check statistics for single categorical value
  expect_match(codebook, "All Categories:<br>X = 5 \\(100.0%\\)")

  # Check statistics for single date value
  expect_match(codebook, "Min: 2021-01-01")
  expect_match(codebook, "Max: 2021-01-01")

  # Check statistics for single character value
  expect_match(codebook, "All Categories:<br>X = 5 \\(100.0%\\)")

  # Check statistics for single logical value
  expect_match(codebook, "TRUE: 5 \\(100.0%\\)")
})

test_that("generate_codebook handles factors with unused levels", {
  df <- data.frame(
    cat = factor(c("A", "B", "A", "C", "B"), levels = c("A", "B", "C", "D"))
  )

  codebook <- generate_codebook(df)

  # Check that only used levels are counted
  expect_match(codebook, "A = 2")
  expect_match(codebook, "B = 2")
  expect_match(codebook, "C = 1")

  # Use expect_no_match to ensure "D" is not present
  expect_no_match(codebook, "D = ")
})

test_that("generate_codebook handles factors with all unused levels", {
  df <- data.frame(
    cat = factor(rep(NA, 5), levels = c("A", "B", "C", "D"))
  )

  codebook <- generate_codebook(df)

  # Check that the statistics indicate all values are NA
  expect_match(codebook, "All values are NA")

  # Ensure no categories are listed
  expect_no_match(codebook, "A = ", fixed = TRUE)
  expect_no_match(codebook, "B = ", fixed = TRUE)
  expect_no_match(codebook, "C = ", fixed = TRUE)
  expect_no_match(codebook, "D = ", fixed = TRUE)
})

test_that("generate_codebook handles character variables correctly", {
  df <- data.frame(
    char = c("apple", "banana", "apple", "cherry", "banana", "banana", NA)
  )

  codebook <- generate_codebook(df, top_n = 2)

  # Check that the codebook includes "Top 2 Categories:"
  expect_match(codebook, "Top 2 Categories:")

  # Check statistics for the top categories based on non-NA counts
  expect_match(codebook, "banana = 3 \\(50.0%\\)")
  expect_match(codebook, "apple = 2 \\(33.3%\\)")

  # Ensure "Hidden" does not appear in the codebook
  expect_no_match(codebook, "Hidden", fixed = TRUE)
})

test_that("generate_codebook correctly calculates missing values", {
  df <- data.frame(
    num = c(1, 2, NA, 4, NA),
    cat = factor(c("A", NA, "A", "B", NA)),
    character = c("X", "Y", "X", NA, "Y"),
    date = as.Date(c("2021-01-01", NA, "2021-03-01", NA, "2021-05-01")),
    logical = c(TRUE, NA, FALSE, NA, TRUE)
  )

  codebook <- generate_codebook(df)

  # Check percentage missing
  expect_match(codebook, "40.0%", fixed = TRUE) # num missing: 2/5 = 40%
  expect_match(codebook, "40.0%", fixed = TRUE) # cat missing: 2/5 = 40%
  expect_match(codebook, "20.0%", fixed = TRUE) # character missing: 1/5 = 20%
  expect_match(codebook, "40.0%", fixed = TRUE) # date missing: 2/5 = 40%
  expect_match(codebook, "40.0%", fixed = TRUE) # logical missing: 2/5 = 40%
})

test_that("generate_codebook correctly rounds statistics", {
  df <- data.frame(
    num = c(1.234, 2.345, 3.456, 4.567, 5.678)
  )

  codebook <- generate_codebook(df)

  # Check that numbers are rounded to one decimal place
  expect_match(codebook, "Min: 1.2")
  expect_match(codebook, "Avg: 3.5")
  expect_match(codebook, "Median: 3.5")
  expect_match(codebook, "Max: 5.7")
  expect_match(codebook, "SD: 1.8")
})

test_that("generate_codebook handles top_n parameter correctly", {
  df <- data.frame(
    cat = factor(c("A", "B", "A", "C", "B", "D", "E", "B", "A"))
  )

  # 1. top_n less than number of categories
  codebook1 <- generate_codebook(df, top_n = 2)

  # Check that the codebook includes "Top 2 Categories:"
  expect_match(codebook1, "Top 2 Categories:")

  # Check that the top 2 categories are "A" and "B" with correct counts and percentages
  expect_match(codebook1, "A = 3 \\(33\\.3%\\)")
  expect_match(codebook1, "B = 3 \\(33\\.3%\\)")

  # Ensure that categories "C", "D", and "E" are **not** present
  expect_no_match(codebook1, "C = 1 \\(11\\.1%\\)", fixed = TRUE)
  expect_no_match(codebook1, "D = 1 \\(11\\.1%\\)", fixed = TRUE)
  expect_no_match(codebook1, "E = 1 \\(11\\.1%\\)", fixed = TRUE)

  # 2. top_n equal to number of categories
  codebook2 <- generate_codebook(df, top_n = 5)

  # Check that the codebook includes "All Categories:"
  expect_match(codebook2, "All Categories:")

  # Check that all categories "A", "B", "C", "D", "E" are present with correct counts and percentages
  expect_match(codebook2, "A = 3 \\(33\\.3%\\)")
  expect_match(codebook2, "B = 3 \\(33\\.3%\\)")
  expect_match(codebook2, "C = 1 \\(11\\.1%\\)")
  expect_match(codebook2, "D = 1 \\(11\\.1%\\)")
  expect_match(codebook2, "E = 1 \\(11\\.1%\\)")

  # 3. top_n greater than number of categories
  codebook3 <- generate_codebook(df, top_n = 10)

  # Check that the codebook includes "All Categories:"
  expect_match(codebook3, "All Categories:")

  # Check that all categories "A", "B", "C", "D", "E" are present with correct counts and percentages
  expect_match(codebook3, "A = 3 \\(33\\.3%\\)")
  expect_match(codebook3, "B = 3 \\(33\\.3%\\)")
  expect_match(codebook3, "C = 1 \\(11\\.1%\\)")
  expect_match(codebook3, "D = 1 \\(11\\.1%\\)")
  expect_match(codebook3, "E = 1 \\(11\\.1%\\)")
})

test_that("generate_codebook can handle variables with mixed types appropriately", {
  # Define var_desc with names matching df columns
  var_desc <- list(
    num = "Numeric variable for testing",
    cat = "Factor variable with categories A, B, C",
    char = "Character variable",
    date = "Date variable for testing",
    logical_var = "Logical variable indicating TRUE or FALSE"
  )

  # Create data frame with correct NAs
  df <- data.frame(
    num = c(1, 2, 3, NA, 5),
    cat = factor(c("A", NA, "A", "B", NA)),
    char = c("X", "Y", "X", NA, "Y"),  # Introduced NA to align with 20% missing
    date = as.Date(c("2021-01-01", NA, "2021-03-01", NA, "2021-05-01")),
    logical_var = c(TRUE, NA, FALSE, NA, TRUE),
    stringsAsFactors = FALSE
  )

  # Generate codebook, hiding 'char' statistics and setting top_n = 3
  codebook <- generate_codebook(df, var_descriptions = var_desc, hide_statistics = "char", top_n = 3)

  # Check that 'char' statistics are hidden
  expect_match(codebook, "char")
  expect_match(codebook, "Hidden")

  # Check that other variables have correct descriptions
  expect_match(codebook, "Numeric variable for testing")
  expect_match(codebook, "Factor variable with categories A, B, C")
  expect_match(codebook, "Date variable for testing")
  expect_match(codebook, "Logical variable indicating TRUE or FALSE")

  # Check specific statistics for 'num'
  expect_match(codebook, "Min: 1.0")
  expect_match(codebook, "Avg: 2.8")
  expect_match(codebook, "Median: 2.5")
  expect_match(codebook, "Max: 5.0")
  expect_match(codebook, "SD: 1.7")

  # Check categorical statistics with top_n = 3
  expect_match(codebook, "All Categories:")

  # Check that 'A' and 'B' are present with correct counts and percentages
  expect_match(codebook, "A = 2 \\(66\\.7%\\)")
  expect_match(codebook, "B = 1 \\(33\\.3%\\)")

  # Ensure that unused categories 'C' and 'D' are not present
  expect_no_match(codebook, "C = ", fixed = TRUE)
  expect_no_match(codebook, "D = ", fixed = TRUE)

  # Check date statistics
  expect_match(codebook, "Min: 2021-01-01")
  expect_match(codebook, "Max: 2021-05-01")

  # Check logical_var statistics
  expect_match(codebook, "TRUE: 2 \\(66\\.7%\\)")
  expect_match(codebook, "FALSE: 1 \\(33\\.3%\\)")
})
