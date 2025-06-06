% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/databook.R
\name{get_categorical_stats}
\alias{get_categorical_stats}
\title{Calculate Categorical Statistics}
\usage{
get_categorical_stats(var, top_n = 5)
}
\arguments{
\item{var}{A factor or character vector representing categorical data.}

\item{top_n}{An integer specifying the number of top categories to display. Default is 5.}
}
\value{
A formatted string listing the top categories with their counts and percentages, or all categories if the number of levels is less than or equal to \code{top_n}.
}
\description{
Helper function to calculate top statistics for categorical variables.
}
\keyword{internal}
