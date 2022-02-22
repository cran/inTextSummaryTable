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
	library(pander)
	library(tools) # toTitleCase


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------
	
	library(clinUtils)

	# load example data
    data(dataADaMCDISCP01)
	
	dataAll <- dataADaMCDISCP01
    labelVars <- attr(dataAll, "labelVars")
	

## ----data-SL------------------------------------------------------------------------------------------------------------------------------------------------------------

	dataSL <- dataAll$ADSL


## ----getHelp, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
#  
#  ? `inTextSummaryTable-stats`
#  

## ----count-simple-------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(data = dataSL)


## ----count-categories---------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(data = dataSL, var = "SEX")


## ----count-categories-order---------------------------------------------------------------------------------------------------------------------------------------------

	# specify manually the order of the categories
	dataSL$SEX <- factor(dataSL$SEX, levels = c("M", "F"))
	getSummaryStatisticsTable(data = dataSL, var = "SEX")
	
	# order categories based on a numeric variable
    dataSL$SEXN <- ifelse(dataSL$SEX == "M", 2, 1)
	dataSL$SEX <- reorder(dataSL$SEX, dataSL$SEXN)
	getSummaryStatisticsTable(data = dataSL, var = "SEX")
	

## ----count-categories-empty-1-------------------------------------------------------------------------------------------------------------------------------------------

	dataSLExample <- dataSL
	
	# 'SEX' formatted as character with only male
	dataSLExample$SEX <- "M" # only male
	getSummaryStatisticsTable(data = dataSLExample, var = "SEX")
	

## ----count-categories-empty-2-------------------------------------------------------------------------------------------------------------------------------------------
	
	# 'SEX' formatted as factor, to include also female in the table
	# (even if not available in the data)
	dataSLExample$SEX <- factor("M", levels = c("F", "M"))
	getSummaryStatisticsTable(data = dataSLExample, var = "SEX", varInclude0 = TRUE)
	# or:
    getSummaryStatisticsTable(data = dataSLExample, var = "SEX", varInclude0 = "SEX")


## ----flag-variables-----------------------------------------------------------------------------------------------------------------------------------------------------

    labelVars[grep("FL$", colnames(dataSL), value = TRUE)]
    # has the subject discontinued from the study?
    dataSL$DISCONFL


## ----count-flag-var-----------------------------------------------------------------------------------------------------------------------------------------------------

    getSummaryStatisticsTable(
	    data = dataSL,
	    var = "SAFFL"
    )


## ----count-flag-varFlag-------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = "SAFFL",
		varFlag = "SAFFL"
	)


## ----count-varTotalInclude----------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL, 
		var = "SEX", 
		varTotalInclude = TRUE
	)


## ----numeric------------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(data = dataSL, var = "AGE")


## ----mixedTable---------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL, 
		var = c("AGE", "SEX")
	)


## ----getHelpStats, eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------
#  
#      ? `inTextSummaryTable-stats`
#  

## ----stats-count--------------------------------------------------------------------------------------------------------------------------------------------------------

	# count: n, '%' and m
	getSummaryStatisticsTable(
		data = dataSL,
		var = "SEX",
		stats = "count"
	)

	# n (%)
	getSummaryStatisticsTable(
		data = dataSL,
		var = "SEX",
		stats = "n (%)"
	)
	
	# n/N (%)
	getSummaryStatisticsTable(
		data = dataSL,
		var = "SEX",
		stats = "n/N (%)"
	)
	

## ----stats-numeric------------------------------------------------------------------------------------------------------------------------------------------------------
	
	## continuous variable
	
	# all summary stats
	getSummaryStatisticsTable(
		data = dataSL,
		var = "AGE",
		stats = "summary"
	)
	
	# median (range)
	getSummaryStatisticsTable(
		data = dataSL,
		var = "AGE",
		stats = "median (range)"
	)
	
	# median and (range) in a different line:
	getSummaryStatisticsTable(
		data = dataSL,
		var = "AGE",
		stats = "median\n(range)"
	)
	
	# mean (se)
	getSummaryStatisticsTable(
		data = dataSL,
		var = "AGE",
		stats = "mean (se)"
	)
	
	# mean (sd)
	getSummaryStatisticsTable(
		data = dataSL,
		var = "AGE",
		stats = "mean (sd)"
	)


## ----getHelpStats2, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------------
#  
#  ? `inTextSummaryTable-stats`
#  

## ----stats-N------------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("RACE", "SEX"),
		stats = list(N = expression(statN))
	)


## ----stats-meanSE-------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL", "WEIGHTBL", "BMIBL"),
		varGeneralLab = "Parameter", statsGeneralLab = "",
		colVar = "TRT01P",
		stats = list(
			`median` = expression(statMedian),
			`(min, max)` = expression(paste0("(", statMin, ",", statMax, ")"))
		)
	)


## ----getStatsData-------------------------------------------------------------------------------------------------------------------------------------------------------

    # this count table:
    getSummaryStatisticsTable(
	    data = dataSL,
	    var = "SEX",
	    stats = "count"
    )
    # ... is equivalent to:
    getSummaryStatisticsTable(
	    data = dataSL,
	    var = "SEX",
	    stats = getStats(type = "count")
    )

    # this summary table...
    getSummaryStatisticsTable(
	    data = dataSL,
	    var = "AGE",
	    stats = "mean (se)"
    )
    # ... is equivalent to:
    getSummaryStatisticsTable(
	    data = dataSL,
	    var = "AGE",
	    stats = getStatsData(type = "mean (se)", var = "AGE", data = dataSL)[["AGE"]]
    )

## ----statExtra-EachVariable---------------------------------------------------------------------------------------------------------------------------------------------
		
	getSummaryStatisticsTable(
		data = dataSL, 
		var = c("AGE", "RACE"),
		stats = list(
			AGE = getStats("median (range)"),
			RACE = getStats("n (%)")
		)
	)


## ----statsUtilityFct----------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = "HEIGHTBL",
		# specify extra stats to compute
		statsExtra = list(
			statCV = cv,
			statGeomMean = geomMean,
			statGeomSD = geomSD,
			statsGeomCV = geomCV
		)
	)


## ----statsExtra---------------------------------------------------------------------------------------------------------------------------------------------------------

	# include the coefficient of variation via the 'statsExtra' parameter
	getSummaryStatisticsTable(
		data = dataSL,
		var = "HEIGHTBL",
		statsExtra = list(statCVPerc = function(x) sd(x)/mean(x)*100)
	)


## ----statsExtra-stats---------------------------------------------------------------------------------------------------------------------------------------------------

	# format the statistics with the 'stats' parameter
	getSummaryStatisticsTable(
		data = dataSL,
		var = "HEIGHTBL",
		statsExtra = list(statCVPerc = function(x) sd(x)/mean(x)*100),
		stats = list(Mean = expression(statMean), 'CV%' = expression(statCVPerc))
	)



## ----getHelpStats3, eval = FALSE----------------------------------------------------------------------------------------------------------------------------------------
#  
#      ? `inTextSummaryTable-stats`
#  

## ----nDecimals-catVar, echo = FALSE, fig.cap = "Standard Layout for Frequency Tabulations of Categorical Variables<br>"-------------------------------------------------

include_graphics("./images/nDecimals_catVar.png")


## ----getStats-count-----------------------------------------------------------------------------------------------------------------------------------------------------

	# Internal rule for the number of decimals for the percentage
	formatPercentage(c(NA, 0, 100, 99.95, 0.012, 34.768))
	
	# Used by default in the 'getStats' function
	getStats(type = "count")


## ----nDecimals-numVar, echo = FALSE, fig.cap = "Standard Layout for Descriptive Statistics of Continuous Variables<br>"-------------------------------------------------

include_graphics("./images/nDecimals_numVar.png")


## ----getMaxNDecimals----------------------------------------------------------------------------------------------------------------------------------------------------

	# Duration of Disease (Months)
	print(dataSL$DURDIS)
	
	## Extract the number of decimals for each value:
	
	# based on pre-defined rule, this metric should be displayed with 1 decimal:
	getNDecimalsRule(x = dataSL$DURDIS)
	
	# but available in the data only with 0 decimals
	getNDecimalsData(x = dataSL$DURDIS)
	
	# The minimum of the #decimals based on the data and pre-defined rule is:
	getNDecimals(x = dataSL$DURDIS)
	
	## Take the maximum number of decimals 
	getMaxNDecimals(x = dataSL$DURDIS)
	
	## Custom set of statistics are extracted when x is specified:
	getStats(x = dataSL$DURDIS)
	
	# To fix the number of decimals:
	getStats(type = "summary", nDecCont = 1)
	
	## Create summary statistics table
	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "DURDIS"),
		stats = list(
			AGE = getStats(type = "median (range)", x = dataSL$AGE),
            DURDIS = getStats(type = "median (range)", x = dataSL$DURDIS)
		)
	)


## ----stats-digits-------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL"),
		stats = list(
			AGE = list(Median = expression(roundHalfUpTextFormat(statMedian, 1))),
            HEIGHTBL = list(Median = expression(roundHalfUpTextFormat(statMedian, 2)))
		)
	)


## ----stats-digits-complex-----------------------------------------------------------------------------------------------------------------------------------------------

	# wrapper function to include median with specific number of digits
	# and min/max with specified number of digits - 1
	statsDMNum <- function(digitsMin)
		list('Median (range)' = 
			bquote(paste0(
				roundHalfUpTextFormat(statMedian, .(digitsMin+1)), 
				" (", roundHalfUpTextFormat(statMin, .(digitsMin)), ",", 
				roundHalfUpTextFormat(statMax, .(digitsMin)),
				")"
			))
	)

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL", "WEIGHTBL", "BMIBL", "RACE", "SEX"),
		stats = list(
			AGE = statsDMNum(0),
			HEIGHTBL = statsDMNum(1),
			WEIGHTBL = statsDMNum(1),
			BMIBL = statsDMNum(1),
			RACE = getStats("n (%)"),
			SEX = getStats("n (%)")
		)
	)


## ----statsLayoutRow-----------------------------------------------------------------------------------------------------------------------------------------------------

	# statsLayout = 'row'
	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL"),
		stats = list(Mean = expression(statMean), 'SE' = expression(statSE))
	)
	

## ----statsLayoutCol-----------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL"),
		stats = list(Mean = expression(statMean), 'SE' = expression(statSE)),
		statsLayout = "col"
	)


## ----summaryTable-PP-medianMinMax-statsLayoutRowVarInSepCol-------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL"),
		stats = list(Mean = expression(statMean), 'SE' = expression(statSE)),
		statsLayout = "rowInSepCol"
	)


## ----summaryTable-statsLayout-onlyOneStat-------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL"),
		stats = list(Mean = expression(statMean))
	)


## ----summaryTable-statsLayout-onlyOneStat-statsLabInclude---------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataSL,
		var = c("AGE", "HEIGHTBL"),
		stats = list(Mean = expression(statMean)),
		statsLabInclude = TRUE
	)


## ----countTable-AE-data-------------------------------------------------------------------------------------------------------------------------------------------------
				
	dataAE <-  subset(dataAll$ADAE, SAFFL == "Y" & TRTEMFL == "Y")
	
	# ensure that order of elements is the one specified in 
	# the corresponding numeric variable
	dataAE$TRTA <- with(dataAE, reorder(TRTA, TRTAN))
	dataAE$AESEV <- factor(
		dataAE$AESEV, 
		levels = c("MILD", "MODERATE", "SEVERE")
	)
	
	dataAEInterest <- subset(dataAE, AESOC %in% c(
		"INFECTIONS AND INFESTATIONS",
        "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS"
       )
    )


## ----rowVarColVar-------------------------------------------------------------------------------------------------------------------------------------------------------

	# unique row variable
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = "AEDECOD",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	
	# multiple nested row variables
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	
	# unique column variable
	getSummaryStatisticsTable(
		data = dataAEInterest,
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	
	# combination of rows and columns
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars,
		colHeaderTotalInclude = FALSE
	)


## ----rowVarInSepCol-----------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD", "AESEV"),
		rowVarInSepCol = "AESEV",
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	

## ----rowOrder-common----------------------------------------------------------------------------------------------------------------------------------------------------

	# 'auto':

	# set order of SOC to reverse alphabetical order
	dataAEInterest$AESOC <- factor(
		dataAEInterest$AESOC, 
		levels = rev(sort(unique(as.character(dataAEInterest$AESOC))))
	)
	# AEDECOD is not a factor -> sort alphabetically by default
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarLab = labelVars[c("AEDECOD")],
		rowVarTotalInclude = c("AESOC", "AEDECOD"),
		colVar = "TRTA", colTotalInclude = TRUE,
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	
	# total counts
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarLab = labelVars[c("AEDECOD")],
		rowVarTotalInclude = c("AESOC", "AEDECOD"),
		colVar = "TRTA", colTotalInclude = TRUE, colTotalLab = "Number of subjects",
		rowOrder = "total",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	# same order even if the 'total' column is not specified
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarLab = labelVars[c("AEDECOD")],
		rowVarTotalInclude = c("AESOC", "AEDECOD"),
		colVar = "TRTA", 
		rowOrder = "total", 
		stats = getStats("n (%)"),
		labelVars = labelVars
	)


## ----rowOrder-specific--------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarLab = labelVars[c("AEDECOD")],
		rowVarTotalInclude = c("AESOC", "AEDECOD"),
		colVar = "TRTA", #colTotalInclude = TRUE,
		rowOrder = c(AESOC = "alphabetical", AEDECOD = "total"),
		stats = getStats("n (%)"),
		labelVars = labelVars
	)


## ----rowOrder-rowOrderTotalFilterFct------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarLab = labelVars[c("AEDECOD")],
		rowVarTotalInclude = c("AESOC", "AEDECOD"),
		colVar = "TRTA", colTotalInclude = TRUE,
		rowOrder = "total",
		stats = getStats("n (%)"),
		labelVars = labelVars,
		# consider only the counts of the treated patients to order the rows
		rowOrderTotalFilterFct = function(x) subset(x, TRTA == "Xanomeline High Dose")
	)


## ----rowOrder-functionExample1------------------------------------------------------------------------------------------------------------------------------------------

	library(plyr)
	getSummaryStatisticsTable(
		data = dataAEInterest,
		type = "count",
		rowVar = "AEHLT",
		rowOrder = function(x){
			x <- subset(x, !isTotal)
			totalAcrossTreatments <- subset(x, TRTA == "Total")
			# counts across treated patients
			totalForTreatmentOnly <- subset(x, TRTA == "Xanomeline High Dose")
			dataCounts <- merge(totalAcrossTreatments, totalForTreatmentOnly, by = "AEHLT", suffixes = c(".all", ".treat"))
			# sort first based on overall count, then counts of treated patients
			dataCounts[with(dataCounts, order(`statN.all`, `statN.treat`, decreasing = TRUE)), "AEHLT"]
		},
		colVar = "TRTA", colTotalInclude = TRUE,
		labelVars = labelVars,
		title = "Table: Adverse Events ordered based on total counts",
		stats = list(expression(paste0(statN, " (", round(statPercN, 1), ")"))),
		footer = "Statistics: n (%)"
	)


## ----rowOrder-functionExample2------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarLab = labelVars[c("AEDECOD")],
		rowVarTotalInclude = c("AESOC", "AEDECOD"),
		colVar = "TRTA", colTotalInclude = TRUE,
		rowOrder = list(
			AESOC = function(table) {
				# records with total for each AESOC
				nAESOCPlacebo <- subset(table, !isTotal & grepl("placebo", TRTA) & AEDECOD == "Total")
				nAESOCTreat <- subset(table, !isTotal & grepl("High Dose", TRTA) & AEDECOD == "Total")
				nAESOCDf <- merge(nAESOCPlacebo, nAESOCTreat, by = "AESOC", suffixes = c(".placebo", ".treatment"))
				nAESOCDf[with(nAESOCDf, order(`statN.placebo`, `statN.treatment`, decreasing = TRUE)), "AESOC"]
			},
			AEDECOD = function(table) {
				# records with counts for each AEDECOD
				nAEDECODPlacebo <- subset(table, !isTotal & grepl("placebo", TRTA) & AEDECOD != "Total")
				nAEDECODTreat <- subset(table, !isTotal & grepl("High Dose", TRTA) & AEDECOD != "Total")
				nAEDECODDf <- merge(nAEDECODPlacebo, nAEDECODTreat, by = "AEDECOD", suffixes = c(".placebo", ".treatment"))
				nAEDECODDf[with(nAEDECODDf, order(`statN.placebo`, `statN.treatment`, decreasing = TRUE)), "AEDECOD"]
			}
		),
		stats = getStats("n (%)"),
		labelVars = labelVars
	)


## ----summaryTable-PP-rowVarWithLabel-labelVars--------------------------------------------------------------------------------------------------------------------------

	# combination of rows and columns
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	

## ----summaryTable-PP-rowVarWithLabel-rowVarLab--------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		stats = getStats("n (%)"),
		rowVarLab = c(
			'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
		),
		labelVars = labelVars
	)
	

## ----countTable-emptyVars-----------------------------------------------------------------------------------------------------------------------------------------------

	## only consider a subset of adverse events
	dataAESubset <- subset(dataAE, AEHLT == "HLT_0617")
	
	## create dummy categories for:
	# treatment
	dataAESubset$TRTA <- with(dataAESubset, 
		factor(TRTA, levels = c(unique(as.character(TRTA)), "Treatment B"))
	)
	# low-level term category
	dataAESubset$AELLT <- with(dataAESubset, 
		factor(AELLT, levels = c(unique(as.character(AELLT)), "Lymphocyte percentage increased"))
	)
	
	# create summary statistics table
	getSummaryStatisticsTable(
		data = dataAESubset,
		type = "count",
		rowVar = c("AEHLT", "AELLT"),
		rowInclude0 = TRUE, colInclude0 = TRUE,
		colVar = "TRTA",
		labelVars = labelVars,
		title = "Table: Adverse Events: white blood cell analyses",
		stats = getStats("n (%)"),
		footer = "Statistics: n (%)"
	)


## ----summaryTable-layout-var--------------------------------------------------------------------------------------------------------------------------------------------

    dataDIABP <- subset(dataAll$ADVS, 
		SAFFL == "Y" & ANL01FL == "Y" &
		PARAMCD == "DIABP" & 
		AVISIT %in% c("Baseline", "Week 8") &
		ATPT == "AFTER LYING DOWN FOR 5 MINUTES"
	)
    dataDIABP$TRTA <- reorder(dataDIABP$TRTA, dataDIABP$TRTAN)
    dataDIABP$AVISIT <- reorder(dataDIABP$AVISIT, dataDIABP$AVISITN)
    
	getSummaryStatisticsTable(
		data = dataDIABP,
		var = c("AVAL", "CHG"),
		colVar = "TRTA",
		rowVar = "AVISIT",
		labelVars = labelVars,
		stats = getStats("summary-default")
	)


## ----summaryTable-layout-varInColumn------------------------------------------------------------------------------------------------------------------------------------
    
getSummaryStatisticsTable(
	data = dataDIABP,
	var = c("AVAL", "CHG"),
	colVar = c("variable", "TRTA"),
	rowVar = "AVISIT",
	labelVars = labelVars,
	stats = getStats("summary-default")
)


## ----summaryTable-layout-var-oneLabel-default---------------------------------------------------------------------------------------------------------------------------
		
	getSummaryStatisticsTable(data = dataSL, var = "AGE", colVar = "TRT01P")


## ----summaryTable-layout-var-oneLabel-varLabInclude---------------------------------------------------------------------------------------------------------------------
		
	getSummaryStatisticsTable(
		data = dataSL, 
		var = "AGE", 
		varLabInclude = TRUE,
		colVar = "TRT01P"
	)


## ----summaryTable-layout-var-empty--------------------------------------------------------------------------------------------------------------------------------------

    dataAEInterest$AESEVN <- ifelse(dataAEInterest$AESEV == "MILD", 1, 2)
	dataAEInterestWC <- ddply(dataAEInterest, c("AEDECOD", "USUBJID", "TRTA"), function(x) {
		x[which.max(x$AESEVN), ]
	})
	dataAEInterestWC[1, "AESEV"] <- NA
    getSummaryStatisticsTable(
        data = dataAEInterestWC,
        colVar = "TRTA",
        rowVar = "AEBODSYS",
		stats = getStats("n (%)"),
        var = c("AESEV", "all"),
		labelVars = labelVars
    )


## ----header-total-default-----------------------------------------------------------------------------------------------------------------------------------------------
	
	# by default, total number of subjects extracted from data
	getSummaryStatisticsTable(
		data = subset(dataAEInterest, AESOC == "INFECTIONS AND INFESTATIONS"),
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		stats = getStats("n (%)"),
		rowVarLab = c(
			'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
		),
		labelVars = labelVars
	)
	

## ----header-total-dataTotal---------------------------------------------------------------------------------------------------------------------------------------------
	
	# dataset used to extract the 'Total'
	dataTotalAE <- subset(dataAll$ADSL, SAFFL == "Y")
	# should contain columns specified in 'colVar'
	dataTotalAE$TRTA <- dataTotalAE$TRT01A 

	getSummaryStatisticsTable(
		data = subset(dataAEInterest, AESOC == "INFECTIONS AND INFESTATIONS"),
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		stats = getStats("n (%)"),
		rowVarLab = c(
			'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
		),
		dataTotal = dataTotalAE,
		labelVars = labelVars
	)


## ----header-total-colHeaderTotalInclude---------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = subset(dataAEInterest, AESOC == "INFECTIONS AND INFESTATIONS"),
		rowVar = c("AESOC", "AEDECOD"),
		rowVarTotalInclude = "AEDECOD",
		rowVarTotalInSepRow = "AEDECOD",
		colVar = "TRTA",
		stats = getStats("n (%)"),
		rowVarLab = c(
			'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
		),
		dataTotal = dataTotalAE,
		labelVars = labelVars,
		colHeaderTotalInclude = FALSE
	)


## ----dataTotalPerc------------------------------------------------------------------------------------------------------------------------------------------------------

    getSummaryStatisticsTable(
	    data = subset(dataAEInterest, AESOC == "INFECTIONS AND INFESTATIONS"),
	    rowVar = c("AESOC", "AEDECOD"),
	    colVar = "TRTA",
	    stats = getStats("n (%)"),
	    rowVarLab = c(
		    'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
	    ),
	    dataTotalPerc = dataTotalAE,
	    labelVars = labelVars
    )


## ----rowVarTotalPerc----------------------------------------------------------------------------------------------------------------------------------------------------

	dataLB <- subset(dataAll$ADLBC, 
		SAFFL == "Y" & 
		PARAMCD %in% c("K", "CHOL") &
		grepl("(Baseline)|(Week 20)", AVISIT)
	)
	dataLB$AVISIT <- with(dataLB, reorder(trimws(AVISIT), AVISITN))
	
	# counts versus the total per actual treatment arm
	getSummaryStatisticsTable(
		data = dataLB,
		colVar = "TRTA", 
		rowVar = c("PARAM", "AVISIT"), 
		var = "LBNRIND",
		stats = getStats("n (%)"),
		rowAutoMerge = FALSE, emptyValue = "0",
	)
	
	# percentage based on total number of subjects with available
	# measurement at specific visit for each parameter
	getSummaryStatisticsTable(
		data = dataLB,
		colVar = "TRTA", 
		rowVar = c("PARAM", "AVISIT"), 
		rowVarTotalPerc = c("PARAM", "AVISIT"),
		var = "LBNRIND",
		stats = getStats("n (%)"),
		rowAutoMerge = FALSE, emptyValue = "0",
	)	
	

## ----statsPerc----------------------------------------------------------------------------------------------------------------------------------------------------------

getSummaryStatisticsTable(
	data = dataLB,
	colVar = "TRTA", 
	rowVar = c("PARAM", "AVISIT"), 
	rowVarTotalPerc = c("PARAM", "AVISIT"),
	var = "LBNRIND", 
	stats = getStats("m (%)"),
	statsPerc = "statm",
	rowAutoMerge = FALSE, emptyValue = "0",
)


## ----colTotalInclude----------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		colTotalInclude = TRUE, 
		stats = getStats("n (%)"),
		rowVarLab = c(
			'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
		),
		dataTotal = dataTotalAE,
		labelVars = labelVars
	)


## ----colTotalLab--------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		colTotalInclude = TRUE, colTotalLab = "All subjects",
		stats = getStats("n (%)"),
		rowVarLab = c(
			'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
		),
		dataTotal = dataTotalAE,
		labelVars = labelVars
	)


## ----dataTotalCol-------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = subset(dataAEInterest, grepl("High Dose", TRTA)),
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		colTotalInclude = TRUE, colTotalLab = "Placebo and treatment arm",
		dataTotalCol = dataAEInterest,
		stats = getStats("n (%)"), emptyValue = "0",
		rowVarLab = c(
			'AESOC' = "TEAE by SOC and Preferred Term\nn (%)"
		),
		dataTotal = dataTotalAE,
		labelVars = labelVars
	)


## ----rowVarTotalInclude-------------------------------------------------------------------------------------------------------------------------------------------------

	# total reported across AESOC
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarTotalInclude = "AESOC", 
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	
	# total reported across AESOC and across AEDECOD
	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarTotalInclude = c("AESOC", "AEDECOD"), 
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)


## ----rowTotalLab--------------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarTotalInclude = "AESOC", rowTotalLab = "Any AE", 
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)


## ----rowVarTotalInclude-rowVarTotalInSepRow-----------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		rowVarTotalInclude = "AEDECOD",
		rowVarTotalInSepRow = "AEDECOD",
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)


## ----dataTotalRow-------------------------------------------------------------------------------------------------------------------------------------------------------

	dataAEInterest$AESEVN <- as.numeric(dataAEInterest$AESEV)
	
	# compute worst-case scenario per subject*AE term*treatment
	dataAEInterestWC <- ddply(dataAEInterest, c("AESOC", "AEDECOD", "USUBJID", "TRTA"), function(x){
		x[which.max(x$AESEVN), ]
	})

	## datasets used for the total: 
	# for total: compute worst-case across SOC and across AE term
	# (otherwise patient counted in multiple categories if present different categories for different AEs)
	dataTotalRow <- list(
		# within visit (across AEDECOD)
		'AEDECOD' = ddply(dataAEInterest, c("AESOC", "USUBJID", "TRTA"), function(x){	
			x[which.max(x$AESEVN), ]
		}),
		# across visits
		'AESOC' = ddply(dataAEInterest, c("USUBJID", "TRTA"), function(x){	
			x[which.max(x$AESEVN), ]
		})
	)

	getSummaryStatisticsTable(
		data = dataAEInterestWC,
		## row variables:
		rowVar = c("AESOC", "AEDECOD", "AESEV"), 
		rowVarInSepCol = "AESEV",
		# total for column header and denominator
		dataTotal = dataTotalAE, 
		# include total across SOC and across AEDECOD
		rowVarTotalInclude = c("AESOC", "AEDECOD"), 
		# data for total row
		dataTotalRow = dataTotalRow, 
		# count for each severity category for the total
		rowVarTotalByVar = "AESEV", 
		rowTotalLab = "Any TEAE", 
		rowVarLab = c(AESOC = "Subjects with, n(%):", AESEV = "Worst-case scenario"),
		# sort per total in the total column
		rowOrder = "total", 
		## column variables
		colVar = "TRTA", 
		stats = getStats("n (%)"),
		emptyValue = "0",
		labelVars = labelVars
	)


## ----titleAndFooter-----------------------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		stats = getStats("n (%)"),
		dataTotal = dataTotalAE,
		labelVars = labelVars,
		title = toTitleCase("MOR106-CL-102: Adverse Events by System Organ Class and Preferred Term (Safety Analysis Set, Part 1)"),
		footer = c(
			"N=number of subjects with data; n=number of subjects with this observation",
			"Denominator for percentage calculations = the total number of subjects per treatment group in the safety population"
		)
	)


## ----includeSessionInfo, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------

	pander(sessionInfo())


