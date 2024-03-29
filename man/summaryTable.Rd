% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryTable-doc.R
\name{summaryTable}
\alias{summaryTable}
\title{\code{summaryTable}}
\value{
Not relevant
}
\description{
\enumerate{
\item{The summary statistics are computed in
\code{\link{computeSummaryStatisticsTable}},
which creates a \code{summaryTable} object.}
\item{This object is exported to diverse formats via 
\code{\link{export}}}
}
Multiple \code{summaryTable} objects are combined
together with \code{\link{combine.summaryTable}}.
}
\details{
The \code{summaryTable} is an intermediary 
object of the package. 
This contains the summary statistics as data.frame.
This object contains:
\itemize{
\item{the row and column variable(s)}
\item{the computed statistic(s): \cr
If \code{type} is:
\itemize{
\item{'summaryTable': 
\itemize{
\item{'statN': number of subjects}
\item{'statMean': mean of \code{var}}
\item{'statSD': standard deviation of \code{var}}
\item{'statSE': standard error of \code{var}}
\item{'statMedian': median of \code{var}}
\item{'statMin': minimum of \code{var}}
\item{'statMax': maximum of \code{var}}
\item{'statPerc': percentage of subjects}
\item{'statPercTotalN': total number of subjects based on \code{dataTotalPerc},
denominator of \code{statPerc}}
\item{'statm': number of records}
}
}
\item{'countTable': 
\itemize{
\item{'statN': number of subjects}
\item{'statPercN' (or 'statPercm'): percentage of subjects
(or records depending on \code{statsPerc})}
\item{'statPercTotalN' (or 'statPercTotalm'): total number of 
subjects (or records) based on \code{dataTotalPerc}, and used as
denominator of \code{statPercN} (or 'statPercm')}
\item{'statm': number of records}
}}}
}
\item{computed statistics.\cr
The statistics are stored in columns corresponding
to names of the \code{statsVar}.\cr
If the specified statistics are not named and of length
1, the statistics are stored in a column called: 'Statistic'.}
\item{variables: 
\itemize{
\item{'variable': variable name in case \code{var} is of length > 1}
\item{'variableGroup': in case \code{var} is of length > 1 and for 
variable(s) used for count: elements of the variable}
}
}
\item{'isTotal': variable with logical flag, TRUE if the record contain 
the total by column}
}
Additionally, the output contains an extra attribute
'summaryTable', which is a list composed of:
\itemize{
\item{'statsVar': column name(s) of summary table with computed statistics
included in the final table}
\item{'rowVar': column name(s) of summary table with row variable
included in the final table. This parameter should be mainly used for qualitative variables and 'nests' together different rows in the final output table.}
\item{'rowVarLab': labels corresponding to the 'rowVar' attribute}
\item{'rowVarTotalInclude': row variables whose total will be included:
\code{rowVarTotalInclude} and 'variableGroup' if the variable total should be included}
\item{'rowVarTotalInSepRow': row variables whose total will be included in a separated row:
\code{rowVarTotalInSepRow} and 'variableGroup' if \code{varTotalInSepRow}}
\item{'colVar': column name(s) of summary table with column variable
included in the final table}
\item{'colTotalLab': label for the total}
}
}
