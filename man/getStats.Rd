% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats-getStats.R
\name{getStats}
\alias{getStats}
\title{Get default set of statistics for one particular variable.}
\usage{
getStats(
  type = "summary",
  includeName = TRUE,
  x = NULL,
  nDecCont = getMaxNDecimals,
  nDecN = 0,
  nDecm = nDecN,
  formatPercentage = inTextSummaryTable::formatPercentage
)
}
\arguments{
\item{type}{Character vector with type of statistics (multiple are possible).
Available statistics are specified in the section 'Formatted statistics' and
formatting in 'Statistics formatting' in
\code{\link[=inTextSummaryTable-stats]{in-text table statistics}}.}

\item{includeName}{Logical, should the statistics name be included (TRUE by default)?
This is applied for the statistic names used in each for the set defined in \code{type};
and for the label of the list if \code{type} is of length 2.
If there are multiple \code{type} or statistics within a set, the names are retained (to avoid confusion).}

\item{x}{(optional, recommended for continuous variable) Numeric vector 
for which the statistics should be computed on.\cr
This is used to derive the number of decimals to include
for a continuous variable.\cr
If not specified, the values are rounded with \code{\link{formatC}}.}

\item{nDecCont}{Integer with base number of decimals 
for continuous variable, or function returning this number based on \code{x} 
(\code{\link{getNDecimals}} by default).}

\item{nDecN, nDecm}{Integer with number of decimals 
for number of subjects/records (0 by default).}

\item{formatPercentage}{Function used to format the percentages
(see \code{\link{formatPercentage}} for default behaviour).}
}
\value{
Expression (or call object) containing
function to extract requested summary statistics.
If multiple \code{type} are specified, they are combined to a list.
Names of the list will be typically used to name the statistic
in the summary table.
}
\description{
This set of statistics can be passed directly to the \code{stats} parameter
of the of the package functions.
}
\examples{
## default set of statistics are available for:

# for count table:
getStats("count")
getStats("n (\%)")
getStats("n")
getStats("\%")
getStats("m")
getStats("\%m")
getStats("m (\%)")
# for continuous variable:
getStats("summary")
getStats("mean (se)")
getStats("mean (sd)")
getStats("median (range)")
getStats("median\n(range)")
getStats(c("Mean", "SE"))

## to not include statistic name in the table
getStats("median\n(range)", includeName = FALSE)
getStats(c("summary", "median\n(range)"), includeName = FALSE)

## to extract the number of decimals based on a continuous variable (see ?getMaxNDecimals) 
exampleData <- data.frame(
  USUBJID = 1 : 4, 
  WEIGHT = c(67, 78, 83, 61), 
  SEX = c("F", "M", "M", "F"), 
  stringsAsFactors = FALSE
)
getStats(type = c('median (range)', 'mean (se)'), x = exampleData$WEIGHT)
# compare with when 'x' is not specified:
getStats(type = c('median (range)', 'mean (se)'))

## custom function to format the percentages:
getStats(type = "count", formatPercentage = function(x) round(x, 2))
}
\seealso{
\link{getStatsData}
}
\author{
Laure Cougnaud
}
