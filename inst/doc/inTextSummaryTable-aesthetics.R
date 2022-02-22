## ----options, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------

library(knitr)
opts_chunk$set(
    echo = TRUE, results = 'markup', warning = FALSE, 
    # stop document execution if error (not the default)
    error = FALSE, 
    message = FALSE, cache = FALSE,
    fig.width = 4, fig.height = 4,
    fig.path = "./figures_vignette/",
    fig.align = 'center')
options(width = 170)
# instead of warn = 0 by default
# include warnings when they occur in the document
options(warn = 1)


## ----aesthetics-showOptions---------------------------------------------------------------------------------------------------------------------------------------------

library(inTextSummaryTable)

# options for color scheme in in presentations
options("inTextSummaryTable.colors.table.presentation")

# options for colors in plots: viridis palette
options("inTextSummaryTable.colors.plot")

# options for shapes in plots
options("inTextSummaryTable.shapes.plot")

# options for linetypes in plots
options("inTextSummaryTable.linetypes.plot")


## ----aesthetics-defaultsReporting---------------------------------------------------------------------------------------------------------------------------------------

# default report style
getSummaryStatisticsTable(
    data = data.frame(USUBJID = c(1, 2)),
    style = "report" 
)

# default presentation style
getSummaryStatisticsTable(
    data = data.frame(USUBJID = c(1, 2)),
    style = "presentation" 
)


## ----aesthetics-changeReportingOptions----------------------------------------------------------------------------------------------------------------------------------

# create named vector
customColorTable <- c(
    # black text in the header
    'header' = "#000000",
    # green background in the header
    'headerBackground' = "#74D055FF",
    # black text in the body
    'body' = "#000000", 
    # yellow background for all rows
    'bodyBackground1' = "#FDE725FF",
    'bodyBackground2' = "#FDE725FF",
    # black footer
    'footer' = "#000000",
    # white footer background
    'footerBackground' = "#FFFFFF",
    # black line for footer
    'line' = "#000000"
)
# set options
options(inTextSummaryTable.colors.table.presentation = customColorTable)
# create the table
getSummaryStatisticsTable(
    data = data.frame(USUBJID = c(1, 2)),
    style = "presentation" 
)


## ----aesthetics-changeDimPageOptions------------------------------------------------------------------------------------------------------------------------------------

# default page dimension of a powerpoint created with Rmd
getOption("inTextSummaryTable.pageDim.presentation")

# set custom dimension of page for presentation
# in this example, the dimension is the widescreen size
pageDimCustom <- c(7.5, 13.32)
options(inTextSummaryTable.pageDim.presentation = pageDimCustom)
getOption("inTextSummaryTable.pageDim.presentation")


## ----getVignetteClinUtils, eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
#  
#  vignette("clinUtils-vignette", "clinUtils")
#  

## ----aesthetics-defaultsVisualization-----------------------------------------------------------------------------------------------------------------------------------

# default colors, shapes and linetypes
summaryTable <- data.frame(
    visit = c(1, 2, 1, 2), 
    TRT = c("A", "A", "B", "B"),
    statMean = rnorm(4)
)    
subjectProfileSummaryPlot(
    data = summaryTable,
    xVar = "visit", 
    colorVar = "TRT" 
)


## ----aesthetics-changeVisualizationOptions------------------------------------------------------------------------------------------------------------------------------

# specify colors
options(inTextSummaryTable.colors.plot = c("red", "green"))
subjectProfileSummaryPlot(
    data = summaryTable,
    xVar = "visit", 
    colorVar = "TRT" 
)

# specify shape
options(inTextSummaryTable.shapes.plot = c("circle", "square"))
subjectProfileSummaryPlot(
    data = summaryTable,
    xVar = "visit", 
    colorVar = "TRT" 
)

# specify linetypes
options(inTextSummaryTable.linetypes.plot = c("dotdash", "longdash"))
subjectProfileSummaryPlot(
    data = summaryTable,
    xVar = "visit", 
    colorVar = "TRT" 
)


## ----aesthetics-backDefaultPalettes-------------------------------------------------------------------------------------------------------------------------------------

options(inTextSummaryTable.colors.table.presentation = tableColorsPresentation)
options(inTextSummaryTable.colors.plot = clinUtils::clinColors)
options(inTextSummaryTable.shapes.plot = clinUtils::clinShapes)
options(inTextSummaryTable.linetypes.plot = clinUtils::clinLinetypes)


## ----aesthetics-getDimPage----------------------------------------------------------------------------------------------------------------------------------------------

# a4 format with one 1 inch margin
getDimPage(type = "width", style = "report")
getDimPage(type = "height", style = "report")

# Presentation format (according to template) with one 1 inch margin
getDimPage(type = "width", style = "presentation")
getDimPage(type = "height", style = "presentation")


## ----includeSessionInfo, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------

library(pander)
pander(sessionInfo())


