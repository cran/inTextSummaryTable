## ----options, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------

library(knitr)
opts_chunk$set(
    echo = TRUE, 
    results = 'markup', warning = FALSE, 
    # stop document execution if error (not the default)
    error = FALSE, 
    message = FALSE, cache = FALSE,
    fig.width = 8, fig.height = 7,
    fig.path = "./figures_vignette/",
    fig.align = 'center')
options(width = 170)
# instead of warn = 0 by default
# include warnings when they occur in the document
options(warn = 1)


## ----loadPackages-------------------------------------------------------------------------------------------------------------------------------------------------------
library(inTextSummaryTable)
library(clinUtils)

## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------

# load example data
data(dataADaMCDISCP01)

dataAll <- dataADaMCDISCP01
labelVars <- attr(dataAll, "labelVars")


## ----visualization-extractData------------------------------------------------------------------------------------------------------------------------------------------

dataLB <- subset(dataAll$ADLBC, grepl("Baseline|Week", AVISIT))
dataLB$AVISIT <- with(dataLB, reorder(trimws(AVISIT), AVISITN))
dataLB$TRTA <- with(dataLB, reorder(TRTA, TRTAN))

summaryTableDf <- computeSummaryStatisticsTable(
    data = dataLB,
    var = "AVAL",
    rowVar = c("PARCAT1", "PARAM"),
    colVar = c("TRTA", "AVISIT")
)


## ----visualization-a4document-------------------------------------------------------------------------------------------------------------------------------------------

# create the plot
dataPlot <- subset(
    summaryTableDf, 
    !isTotal &
     PARAM == "Alanine Aminotransferase (U/L)"
)

subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)


## ----visualization-presentation, eval = requireNamespace("extrafont", quietly = TRUE) && "Tahoma" %in% extrafont::fonts()-----------------------------------------------
# # 'Tahoma' font should be registered upfront to create plots with: 'presentation' style
# subjectProfileSummaryPlot(
#     data = dataPlot,
#     xVar = "AVISIT",
#     colorVar = "TRTA",
#     labelVars = labelVars,
#     useLinetype = TRUE,
#     tableText = "statN",
#     style = "presentation"
# )
# 

## ----getVignetteAesthetics, eval = FALSE--------------------------------------------------------------------------------------------------------------------------------
# 
# vignette("inTextSummaryTable-aesthetics", "inTextSummaryTable")
# 

## ----getVignetteClinUtils, eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
# 
# vignette("clinUtils-vignette", "clinUtils")
# 

## ----visualization-customColorFromArgument------------------------------------------------------------------------------------------------------------------------------

# custom color palette by setting a named vector of colors
customColorPalette <- c(
    `Xanomeline Low Dose` = "green",
    `Xanomeline High Dose` = "red",
	`Placebo` = "blue"
)
subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    colorPalette = customColorPalette,
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)


## ----visualization-customShapeFromArgument------------------------------------------------------------------------------------------------------------------------------

# custom shape palette
customShapePalette <- c(15, 17, 19)
subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    shapePalette = customShapePalette,
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)

# custom linetype palette
customLinetypePalette <- c("twodash", "dotted", "longdash")
subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    linetypePalette = customLinetypePalette,
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)


## ----exampleLoadingPackageAndOptions, eval = FALSE----------------------------------------------------------------------------------------------------------------------
# 
# # this is OK
# library(inTextSummaryTable)
# options(inTextSummaryTable.colors.plot = customColorPalette)
# 
# # this does NOT set your custom palette
# options(inTextSummaryTable.colors.plot = customColorPalette)
# library(inTextSummaryTable)
# 

## ----visualization-customColorFromOptions-------------------------------------------------------------------------------------------------------------------------------

# custom color palette by setting a simple vector of colors
customColorPalette <- c("blue", "green", "orange")
options(inTextSummaryTable.colors.plot = customColorPalette)

subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)

# custom color palette by setting a named vector of colors
customColorPalette <- c(
    `Xanomeline Low Dose` = "green",
    `Xanomeline High Dose` = "red",
	`Placebo` = "purple"
)
options(inTextSummaryTable.colors.plot = customColorPalette)

subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)


## ----visualization-customShapeFromOptions-------------------------------------------------------------------------------------------------------------------------------

# custom shape palette
customShapePalette <- c(17, 19, 22)
options(inTextSummaryTable.shapes.plot = customShapePalette)

subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)

# custom linetype palette
customLinetypePalette <- c("dotted", "longdash", "solid")
options(inTextSummaryTable.linetypes.plot = customLinetypePalette)

subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)


## ----visualization-backDefaultPalettes----------------------------------------------------------------------------------------------------------------------------------

options(inTextSummaryTable.colors.plot = clinColors)
options(inTextSummaryTable.shapes.plot = clinShapes)
options(inTextSummaryTable.linetypes.plot = clinLinetypes)


## ----visualization-table------------------------------------------------------------------------------------------------------------------------------------------------

subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = NULL
)


## ----visualization-facets, out.width = "100%", fig.height = 9, fig.width = 9--------------------------------------------------------------------------------------------

# create the plot
dataPlotFacets <- subset(
    summaryTableDf, 
    !isTotal &
        PARAM %in% c(
            "Alanine Aminotransferase (U/L)",
            "Albumin (g/L)",
            "Bilirubin (umol/L)",
            "Calcium (mmol/L)"
        )
)

subjectProfileSummaryPlot(
    data = dataPlotFacets,
    xVar = "AVISIT",
    colorVar = "TRTA",
    labelVars = labelVars,
    facetVar = c("PARCAT1", "PARAM"),
    useLinetype = TRUE
)


## ----visualization-otherInputs------------------------------------------------------------------------------------------------------------------------------------------

# plot the median instead of mean
# no error bars
subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    meanVar = "statMedian",
    seVar = NULL,
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)

# plot the mean with standard deviation
subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    seVar = "statSD",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)

# change labels
subjectProfileSummaryPlot(
    data = dataPlot,
    xVar = "AVISIT",
    xLab = "Time points",
    yLab = "Mean and Standard Errors",
    title = "Title of the plot",
    colorVar = "TRTA",
    labelVars = labelVars,
    useLinetype = TRUE,
    tableText = "statN"
)


## ----includeSessionInfo, echo = FALSE, results = "asis"-----------------------------------------------------------------------------------------------------------------
print(sessionInfo())

