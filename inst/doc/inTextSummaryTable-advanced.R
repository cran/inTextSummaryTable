## ----getVignette, eval = FALSE------------------------------------------------
#  
#  vignette("inTextSummaryTable-createTables", "inTextSummaryTable")
#  vignette("inTextSummaryTable-exportTables", "inTextSummaryTable")
#  

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
library(clinUtils)
library(pander)
library(tools) # toTitleCase


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------

# load example data
data(dataADaMCDISCP01)

dataAll <- dataADaMCDISCP01
labelVars <- attr(dataAll, "labelVars")


## ----formatExampleData--------------------------------------------------------------------------------------------------------------------------------------------------

dataAE <-  subset(dataAll$ADAE, SAFFL == "Y" & TRTEMFL == "Y")
dataAEInterest <- subset(dataAE, AESOC %in% c(
        "INFECTIONS AND INFESTATIONS",
        "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS"
    )
)

# ensure that order of elements is the one specified in 
# the corresponding numeric variable
dataAEInterest$TRTA <- reorder(dataAEInterest$TRTA, dataAEInterest$TRTAN)
dataAEInterest$AESEV <- factor(dataAEInterest$AESEV, levels = c("MILD", "MODERATE"))

dataTotalAE <- subset(dataAll$ADSL, TRT01A != "Placebo")
# should contain columns specified in 'colVar'
dataTotalAE$TRTA <- dataTotalAE$TRT01A 


## ----computeSummaryStatisticsTable--------------------------------------------------------------------------------------------------------------------------------------

summaryTable <- computeSummaryStatisticsTable(
    data = dataAEInterest,
    rowVar = c("AESOC", "AEDECOD"),
    rowVarTotalInclude = c("AESOC", "AEDECOD"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    dataTotal = dataTotalAE,
    labelVars = labelVars,
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)")
)

pander(head(summaryTable, 3))


## ----computeSummaryStatisticsTable-isTotal------------------------------------------------------------------------------------------------------------------------------

pander(subset(summaryTable, isTotal))


## ----export-flextable---------------------------------------------------------------------------------------------------------------------------------------------------

export(
	summaryTable = summaryTable,
	outputType = "flextable"
)


## ----combine------------------------------------------------------------------------------------------------------------------------------------------------------------
tableDemoCat <- computeSummaryStatisticsTable(
	data = dataADaMCDISCP01$ADSL,
	var = c("SEX", "AGE"), varInclude0 = TRUE,
	colVar = "TRT01P",
	stats = getStats("n (%)", includeName = FALSE),
	labelVars = labelVars
)
tableDemoCont <- computeSummaryStatisticsTable(
	data = dataADaMCDISCP01$ADSL,
	var = c("HEIGHTBL", "WEIGHTBL"),
	colVar = "TRT01P",
	stats = getStats(c("n", "median (range)")),
	labelVars = labelVars
)
tableDemo <- combine(tableDemoCat, tableDemoCont)
export(tableDemo)

## ----combineVariables---------------------------------------------------------------------------------------------------------------------------------------------------

# prepare the data: create grouping of interest
dataAEGroup <- combineVariables(
    data = dataAEInterest,
    newVar = "AEGRP",
    paramsList = list(
        # for all screened patients
        list(var = "TRTA", value = "Xanomeline High Dose"),
        # for moderate severity
        list(var = "AESEV", value = "MODERATE", labelExtra = "Moderate"),
        list(var = "AENDY", label = paste("With adverse events ending date"))
    ),
    # include also counts for all records
    includeAll = TRUE,
    labelAll = "All Adverse events", 
    labelVars = labelVars
)
labelVars["AEGRP"] <- "Patient groups of interest"

# create the table
getSummaryStatisticsTable(
    data = dataAEGroup,
    colVar = "TRTA", 
    rowVar = "AEGRP", 
    labelVars = labelVars,
    dataTotal = dataTotalAE,
    stats = list(expression(paste0(statN, " (", round(statPercN, 1), ")"))),
    title = "Table: Adverse events: counts for groups of interest",
    footer = "Statistics: n (%)"
)


## ----includeSessionInfo, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------

pander(sessionInfo())


