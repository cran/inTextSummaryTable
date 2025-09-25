## ----options, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
	
	library(knitr)
	opts_chunk$set(
		echo = TRUE, results = 'markup', warning = FALSE, 
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
	library(tools) # toTitleCase


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------
	
	library(clinUtils)

	# load example data
    data(dataADaMCDISCP01)
	
	dataAll <- dataADaMCDISCP01
    labelVars <- attr(dataAll, "labelVars")
	
	# dataAll contains a list
	class(dataAll)
	# ... of ADaM datasets
	names(dataAll)
	# access a specific dataset
	head(dataAll$ADSL, 1)
	
	# check label of a subset of the variable(s)
	head(labelVars)
	# or for a specific variable:
	labelVars["USUBJID"]
	# or:
	getLabelVar(var = "USUBJID", labelVars = labelVars)


## ----getVignetteExport, eval = FALSE------------------------------------------------------------------------------------------------------------------------------------
# 
# vignette("inTextSummaryTable-exportTables", "inTextSummaryTable")
# 

## ----getVignetteAdvanced, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------
# 
# vignette("inTextSummaryTable-advanced", "inTextSummaryTable")
# 

## ----getVignetteVisualization, eval = FALSE-----------------------------------------------------------------------------------------------------------------------------
# 
# vignette("inTextSummaryTable-visualization", "inTextSummaryTable")
# 

## ----getVignetteAesthetics, eval = FALSE--------------------------------------------------------------------------------------------------------------------------------
# 
# vignette("inTextSummaryTable-aesthetics", "inTextSummaryTable")
# 

## ----includeSessionInfo, echo = FALSE, results = "asis"-----------------------------------------------------------------------------------------------------------------
print(sessionInfo())

