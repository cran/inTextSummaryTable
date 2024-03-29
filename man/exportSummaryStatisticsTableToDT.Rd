% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DT-exportSummaryStatisticsTableToDT.R
\name{exportSummaryStatisticsTableToDT}
\alias{exportSummaryStatisticsTableToDT}
\title{Export summary table to an interactive DT table,
e.g. to be exported into an html document.}
\usage{
exportSummaryStatisticsTableToDT(
  summaryTable,
  rowVar = getAttribute(summaryTable, "rowVar"),
  rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar,
    labelVars = labelVars)),
  rowVarInSepCol = NULL,
  statsVar = getAttribute(summaryTable, "statsVar"),
  statsLayout = getAttribute(summaryTable, "statsLayout", default = "col"),
  statsValueLab = "StatisticValue",
  title = NULL,
  expandVar = NULL,
  noEscapeVar = NULL,
  barVar = NULL,
  pageDim = NULL,
  labelVars = NULL,
  file = NULL,
  ...
)
}
\arguments{
\item{summaryTable}{Summary table as provided by the 
\code{\link{formatSummaryStatisticsTable}}.}

\item{rowVar}{Character vector with variable(s)
to be included in the rows.
If multiple variables are specified, the variables should 
be sorted in hierarchical order
(e.g. body system class before adverse event term)
and are nested in the table.}

\item{rowVarLab}{Named character vector with 
label for the \code{rowVar} variable(s).}

\item{rowVarInSepCol}{Character vector with \code{rowVar}
that should be included in separated columns.
By default (NULL), all row variables are nested in the first column 
of the table.\cr
To include the groups within a \code{var} variable in a separated column, 
set: \code{rowVarInSepCol == 'variableGroup'}.}

\item{statsVar}{Character vector with columns of \code{summaryTable} with
statistic variables. For the export: if not specified, all columns of \code{data}
besides row, column variables, 'variable', 'variableGroup'
and 'isTotal' are considered.}

\item{statsLayout}{String with layout for the statistics names 
(in case more than one statistic is included), among:
\itemize{
\item{row (by default for 'flextable' output): \cr All statistics are
 included in different rows in the first column of the table (after the row variable(s))}
\item{'col' (by default for 'DT' output): \cr Statistics are included 
in separated columns (last row of the header).\cr
This option is not compatible with categorical variable(s).}
\item{'rowInSepCol': \cr Statistics are included in different rows, 
but in a separated column than the \code{rowVar} variable(s)}
}}

\item{statsValueLab}{String with label for the statistic value, 
'StatisticValue' by default.\cr
This is only included in the table if the statistics provided in 
\code{stats} are not named and if no \code{colVar} is specified.}

\item{title}{Character vector with title(s) for the table.
Set to NULL (by default) if no title should be included.
If multiple are specified, specified for each element of \code{byVar} (in order of the levels).}

\item{expandVar}{(DT output) Character vector with variables of the summary table which
should be expanded in the data.}

\item{noEscapeVar}{(DT output) Character vector with variables of \code{summaryTable}
which shouldn't be escaped in the table (e.g. containing URLs).}

\item{barVar}{(DT output) Character vector with variables of \code{summaryTable}
that should be represented as a bar.}

\item{pageDim}{Numeric vector of length 2 with page width and height.\cr
Depending on \code{outputType}:
\itemize{
\item{'flextable': in inches}
\item{'DT': in number of rows in the table.\cr
Currently only the height is used (e.g. \code{c(NA, 4)})
}}}

\item{labelVars}{(optional) Named character vector with label for
the row, column variable(s) or variable(s) to summarize. \cr
Labels specified via dedicated parameter: e.g. 
\code{rowVarLab}, \code{colVarLab}, \code{varLab}
have priority on this parameter.}

\item{file}{String with path of the file where the table should be exported.
The file should have the extension: '.docx'.
If NULL, the summary table is not exported but only returned as output.
If \code{byVar} is specified, each table is exported to a separated
file with the suffix: 'file_[i].docx' with i the index of the file.}

\item{...}{(DT output) Extra parameters passed to the 
\code{\link[clinUtils]{getClinDT}}}
}
\value{
A \code{\link[DT]{datatable}} object.
}
\description{
Export summary table to an interactive DT table,
e.g. to be exported into an html document.
}
\author{
Laure Cougnaud
}
