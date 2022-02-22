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


## ----getVignette, eval = FALSE------------------------------------------------------------------------------------------------------------------------------------------
#  
#  vignette("inTextSummaryTable-createTables", "inTextSummaryTable")
#  

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


## ----createExampleData--------------------------------------------------------------------------------------------------------------------------------------------------

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



## ----outputType-flextable-----------------------------------------------------------------------------------------------------------------------------------------------

summaryTableFt <- getSummaryStatisticsTable(
    data = dataAEInterest,
    rowVar = c("AESOC", "AEDECOD"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    dataTotal = dataTotalAE,
    labelVars = labelVars,
    title = "Table: Adverse Events by System Organ Class and Preferred Term",
    footer = c(
        "N = number of subjects in the data; n = number of subjects with observation",
        paste("Denominator for percentage calculations is the total number of subjects",
            "per treatment group in the safety population"
        )
    ),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    outputType = "flextable"
)
class(summaryTableFt)
summaryTableFt


## ----style-report-------------------------------------------------------------------------------------------------------------------------------------------------------

getSummaryStatisticsTable(
    data = dataAEInterest,
    rowVar = c("AESOC", "AEDECOD"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    dataTotal = dataTotalAE,
    labelVars = labelVars,
    title = "Table: Adverse Events by System Organ Class and Preferred Term",
    footer = c(
        "N = number of subjects with data; n = number of subjects with this observation",
        paste("Denominator for percentage calculations is the total number of subjects",
            "per treatment group in the safety population"
        )
    ),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)")
)


## ----style-presentation-------------------------------------------------------------------------------------------------------------------------------------------------

getSummaryStatisticsTable(
    data = dataAEInterest,
    rowVar = c("AESOC", "AEDECOD"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    dataTotal = dataTotalAE,
    labelVars = labelVars,
    title = "Table: Adverse Events by System Organ Class and Preferred Term",
    footer = c(
        "N = number of subjects with data; n = number of subjects with this observation",
        paste("Denominator for percentage calculations is the total number of subjects",
            "per treatment group in the safety population"
        )
    ),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    style = "presentation"
)


## ----outputType-DT, eval = rmarkdown::pandoc_available()----------------------------------------------------------------------------------------------------------------

getSummaryStatisticsTable(
    data = dataAEInterest,
    rowVar = c("AESOC", "AEDECOD"),
    colVar = "TRTA",
    stats = getStats("n"),
    dataTotal = dataTotalAE,
    labelVars = labelVars,
    title = "Table: Adverse Events by System Organ Class and Preferred Term",
    footer = c(
        "N = number of subjects in the data; n = number of subjects with observation",
        paste("Denominator for percentage calculations is the total number of subjects",
            "per treatment group in the safety population"
        )
    ),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    outputType = "DT",
	## DT-specific options
	buttons = c() # remove all export buttons
)


## ----outputType-dataframe-----------------------------------------------------------------------------------------------------------------------------------------------

summaryTable <- getSummaryStatisticsTable(
    data = dataAEInterest,
    rowVar = c("AESOC", "AEDECOD"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    dataTotal = dataTotalAE,
    labelVars = labelVars,
    title = "Table: Adverse Events by System Organ Class and Preferred Term",
    footer = c(
        "N = number of subjects in the data; n = number of subjects with observation",
        paste("Denominator for percentage calculations is the total number of subjects",
            "per treatment group in the safety population"
        )
    ),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    style = "presentation",
    outputType = "data.frame"
)
pander(summaryTable, split.table = Inf)


## ----outputType-dataframe-base------------------------------------------------------------------------------------------------------------------------------------------

summaryTableAll <- getSummaryStatisticsTable(
    data = dataAEInterest,
    rowVar = c("AESOC", "AEDECOD"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    dataTotal = dataTotalAE,
    labelVars = labelVars,
    title = "Table: Adverse Events by System Organ Class and Preferred Term",
    footer = c(
        "N = number of subjects in the data; n = number of subjects with observation",
        paste("Denominator for percentage calculations is the total number of subjects",
            "per treatment group in the safety population"
        )
    ),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    style = "presentation",
    outputType = "data.frame-base"
)
pander(summaryTableAll, split.table = Inf)


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
# create the table on a dummy data set
getSummaryStatisticsTable(
    data = data.frame(USUBJID = c(1, 2)),
    style = "presentation" 
)


## ----aesthetics-changeDimPageOptions------------------------------------------------------------------------------------------------------------------------------------

# set custom dimension of page for presentation
# in this example, the dimension is the widescreen size
pageDimCustom <- c(7.5, 13.32)
options(inTextSummaryTable.pageDim.presentation = pageDimCustom)
getOption("inTextSummaryTable.pageDim.presentation")


## ----getVignetteAesthetics, eval = FALSE--------------------------------------------------------------------------------------------------------------------------------
#  
#  vignette("inTextSummaryTable-aesthetics", "inTextSummaryTable")
#  

## ----aesthetics-backDefaultPalettes-------------------------------------------------------------------------------------------------------------------------------------

options(inTextSummaryTable.colors.table.presentation = tableColorsPresentation)


## ----file, eval = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  # export table to a Word document
#  summaryTableFt <- getSummaryStatisticsTable(
#      data = dataAEInterest,
#      rowVar = c("AESOC", "AEDECOD"),
#      colVar = "TRTA",
#      stats = getStats("n (%)"),
#      dataTotal = dataTotalAE,
#      labelVars = labelVars,
#      file = file.path("tables_CSR", "summaryTable-AEs.docx")
#  )
#  
#  # export interactive table to a html document
#  summaryTableFt <- getSummaryStatisticsTable(
#      data = dataAEInterest,
#      rowVar = c("AESOC", "AEDECOD"),
#      colVar = "TRTA",
#      stats = getStats("n (%)"),
#      dataTotal = dataTotalAE,
#      labelVars = labelVars,
#      file = file.path("tables_CSR", "summaryTable-AEs.html")
#  )
#  
#  # export table in raw format to a text file
#  summaryTableFt <- getSummaryStatisticsTable(
#      data = dataAEInterest,
#      rowVar = c("AESOC", "AEDECOD"),
#      colVar = "TRTA",
#      stats = getStats("n (%)"),
#      dataTotal = dataTotalAE,
#      labelVars = labelVars,
#      file = file.path("tables_CSR", "summaryTable-AEs.txt")
#  )
#  
#  # export to multiple formats at once
#  summaryTableFt <- getSummaryStatisticsTable(
#      data = dataAEInterest,
#      rowVar = c("AESOC", "AEDECOD"),
#      colVar = "TRTA",
#      stats = getStats("n (%)"),
#      dataTotal = dataTotalAE,
#      labelVars = labelVars,
#      file = file.path("tables_CSR",
#          c("summaryTable.txt", "summaryTable.docx", "summaryTable.html")
#      )
#  )
#  

## ----export-------------------------------------------------------------------------------------------------------------------------------------------------------------

dataDIABP <- subset(
	dataAll$ADVS, 
	SAFFL == "Y" & ANL01FL == "Y" &
	PARAMCD == "DIABP" & 
	AVISIT %in% c("Baseline", "Week 6") &
	ATPT == "AFTER LYING DOWN FOR 5 MINUTES"
)

# create example of data.frame containing statistics of interest
statsEff <- sapply(c("AVAL", "CHG"), function(var) {
      
      getStats(
          type = c("n", "mean (se)", "median (range)"),
          x = dataDIABP[[var]]
      )
      
    }, simplify = FALSE)

summaryTable <- computeSummaryStatisticsTable(
    data = dataDIABP,
    colVar = c("TRTP", "AVISIT"),
    var = c("AVAL", "CHG"), varGeneralLab = "",
    stats = statsEff,
    labelVars = labelVars
)

# format df with statistics to in-text table format
export(
    summaryTable = summaryTable,
    statsVar = c("n", "Mean (SE)", "Median (range)"),
    rowVar = "variable", rowVarLab = "Statistic",
    colVar = c("TRTP", "AVISIT"),
    colHeaderTotalInclude = TRUE,
    labelVars = simpleCap(tolower(labelVars[c("AVAL", "CHG")])),
    title = toTitleCase(
		"Table: Diastolic Blood Pressure (mmHg) statistics"
    )
)


## ----summaryTable-PP-rowVarWithLabel-rowVarLab-superscript--------------------------------------------------------------------------------------------------------------

dataSL <- subset(dataAll$ADSL, SAFFL == "Y")

varsSL <- c("AGE", "WEIGHTBL", "BMIBL")
labelVars[varsSL] <- c(
    "Age",
    "Weight_{t}",
    "BMI (kg/m^{2})"
)

getSummaryStatisticsTable(
    data = dataSL, 
    var = varsSL,  
    stats = getStats("n (%"),
    labelVars = labelVars,
    fontsize = 16,
    title = toTitleCase("Demographic data (Safety Analysis Set)")
)


## ----summaryTable-PP-rowVarWithLabel-rowVarLab-bold---------------------------------------------------------------------------------------------------------------------

getSummaryStatisticsTable(
    data = dataSL, 
    var = varsSL,  
    stats = list(
		expression(paste0(
			ifelse(statMean > statMedian, 
				paste0("bold{", roundHalfUpTextFormat(statMean, 1), "}"), 
				roundHalfUpTextFormat(statMean, 1)
			), 
			"\n(", 
				roundHalfUpTextFormat(statSE, 2),")")
        )
    ),
    labelVars = labelVars,
    fontsize = 12,
    title = toTitleCase("Demographic data (Safety Analysis Set)")
)


## ----'objectsList1', results = 'asis', echo = FALSE---------------------------------------------------------------------------------------------------------------------
xList[[1]]

## ----'objectsList2', results = 'asis', echo = FALSE---------------------------------------------------------------------------------------------------------------------
xList[[2]]

## ----'objectsList3', results = 'asis', echo = FALSE---------------------------------------------------------------------------------------------------------------------
xList[[3]]

## ----byVar, results = "asis"--------------------------------------------------------------------------------------------------------------------------------------------

summaryTableList <- getSummaryStatisticsTable(
    data = subset(dataAE, TRTEMFL == "Y"),
    rowVar = c("AESOC", "AEDECOD"),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    colVar = "TRTA",
    byVar = "TRTA",
    stats = getStats("n (%)"),
    labelVars = labelVars,
    title = "Table: Treatment-Emergent adverse events"
)

# print the list of tables in the rmarkdown document
clinUtils::knitPrintListObjects(summaryTableList) 


## ----getListing---------------------------------------------------------------------------------------------------------------------------------------------------------

varsListing <- c(
	"USUBJID", "AEBODSYS", "AEDECOD", "TRTA", 
	"AESEV", "AESER", "ASTDY", "AENDY"
)

dataListing <- subset(dataAE, TRTEMFL == "Y" & AESEV == "SEVERE")
dataListing <- dataListing[, varsListing]
colnames(dataListing) <- getLabelVar(var = varsListing, labelVars = labelVars)

getListing(
	data = dataListing, 
	title = "Listing of treatment-emergent severe adverse events",
	includeRownames = FALSE
)


## ----countTable-AE-filterFct--------------------------------------------------------------------------------------------------------------------------------------------

library(plyr)

# SOC with AE terms with at least 2 subjects
getSummaryStatisticsTable(
    data = subset(dataAE, TRTEMFL == "Y"),
    rowVar = c("AESOC", "AEDECOD"),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    filterFct = function(x)
      ddply(x, "AESOC", function(y)
            if(any(y$statN >= 2))	y			
      ),
    labelVars = labelVars,
    title = paste(
        "Table: Adverse Events by System Organ Class and",
        "Preferred Term with at least 2 patients in System Organ Class"
    )
)

# AE term with at least one term with more than 70% in the treatment
getSummaryStatisticsTable(
    data = subset(dataAE, TRTEMFL == "Y"),
    rowVar = c("AESOC", "AEDECOD"),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    filterFct = function(x)
      ddply(x, "AEDECOD", function(xTerm) {
            if(any(xTerm$statPercN >= 70))	xTerm		
          }),
    labelVars = labelVars,
    title = "Table: Adverse Events by System Organ Class and Preferred Term with at least 70% patients"
)


## ----countTable-AE-filterFct- details-----------------------------------------------------------------------------------------------------------------------------------

x <- computeSummaryStatisticsTable(
    data = subset(dataAE, TRTEMFL == "Y"),
    rowVar = c("AESOC", "AEDECOD"),
    rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term\nn (%)"),
    colVar = "TRTA",
    stats = getStats("n (%)"),
    labelVars = labelVars
)
head(x)

# for specific AEDECOD
xTerm <- subset(x, AEDECOD == "MYOCARDIAL INFARCTION")
# identify the record for treated patient:
subset(xTerm, grepl("Placebo", TRTA))
# keep all records (placebo + treatment) if percentage if higher than 20%:
# if no 'else' condition, nothing (NULL) is returned:
if(subset(xTerm, grepl("Placebo", TRTA))$statPercN >= 20)	xTerm
# across all AE terms:
ddply(x, "AEDECOD", function(xTerm)
      if(subset(xTerm, grepl("Placebo", TRTA))$statPercN >= 20)	xTerm
)

# format it as a function and pass it to the 'filterFct' parameter

## ----includeSessionInfo, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------

pander(sessionInfo())


