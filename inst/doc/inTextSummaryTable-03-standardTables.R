## ----options, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
	
	library(knitr)
	opts_chunk$set(
		echo = TRUE, results = 'asis', warning = FALSE, 
		# stop document execution if error (not the default)
		error = FALSE, message = FALSE, cache = FALSE,
		fig.width = 8, fig.height = 7,
		fig.path = "./figures_vignette/",
		fig.align = 'center')
	# instead of warn = 0 by default
	# include warnings when they occur in the document
	options(width = 170)
	

## ----loadPackages, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------

	library(clinUtils)
	library(tools)# toTitleCase
	library(plyr) # for ddply, rbind.fill
	library(inTextSummaryTable)


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------

	# load example data
    library(clinUtils)

    # load example data
    data(dataADaMCDISCP01)

    dataAll <- dataADaMCDISCP01
    labelVars <- attr(dataAll, "labelVars")


## ----table-subjectDisposition-------------------------------------------------------------------------------------------------------------------------------------------
	
	# data of interest
	dataDM <- dataAll$ADSL
	
	varDMFL <- grep("FL$", colnames(dataDM), value = TRUE)
	varDMFLLabel <- sub(" Flag$", "", labelVars[varDMFL])
	
	getSummaryStatisticsTable(
		data = dataDM,
		var = varDMFL, varFlag = varDMFL, varGeneralLab = "Analysis Set, N", 
		varLab = varDMFLLabel,
		stats = getStats("n (%)"),
		colVar = "TRT01P",
		labelVars = labelVars,
		colTotalInclude = TRUE, colTotalLab = "All subjects",
		varInclude0 = TRUE,
		title = toTitleCase("Table: subject disposition"),
		file = file.path("tables_CSR", "Table_subjectDisposition.docx")
	)


## ----table-demography---------------------------------------------------------------------------------------------------------------------------------------------------

	# data of interest
	dataDM <- subset(dataAll$ADSL, SAFFL == "Y")
	
	# variables of interest
	# Note: if available: ethnicity is included
	varsDM <- c(
		"SEX", "AGE", "AGEGR1",
		"RACE", "ETHNIC",
		"HEIGHTBL", "WEIGHTBL", 
		"BMIBL", "BMIBLGR1"
	)

	# Sort variables according to corresponding numeric variable
	dataDM$AGEGR1 <- with(dataDM, reorder(AGEGR1, AGEGR1N))
	dataDM$RACE <- with(dataDM, reorder(RACE, RACEN))
	dataDM$TRT01P <- with(dataDM, reorder(TRT01P, TRT01PN))
	
	## Define set of statistics of interest:
	statsDM <- getStatsData(
		data = dataDM, var = varsDM,
		# different for continuous and categorical variable
		type = c(cont = "median (range)", cat = "n (%)"),
		# for categorical variable, statistic name (here: 'n (%)')
		# should not be included in the table
		args = list(cat = list(includeName = FALSE))
	)

	## create the table:
	
	getSummaryStatisticsTable(
		data = dataDM, 
		# variables to summarize
		var = varsDM, 
		varGeneralLab = "Parameter",
		# column
		colVar = "TRT01P", colTotalInclude = TRUE, colTotalLab = "All subjects",
		# statistics
		stats = statsDM,
		statsGeneralLab = "",
		labelVars = labelVars,
		# if only one category, should be included in separated row (e.g. RACE: White)
		rowAutoMerge = FALSE,
		rowInclude0 = FALSE, emptyValue = 0,
		title = toTitleCase("Table: Demographic Data (safety Analysis Set)"),
		file = file.path("tables_CSR", "Table_demographicData.docx")
	)
	

## ----table-baselineDiseaseCharacteristics-------------------------------------------------------------------------------------------------------------------------------

	# data of interest
	dataBDC <- subset(dataAll$ADSL, SAFFL == "Y")
	
	# create table
	getSummaryStatisticsTable(
		data = dataBDC,
		var = c("DURDIS", "EDUCLVL"), varGeneralLab = "Parameter", 
		colVar = "TRT01P", colTotalInclude = TRUE, colTotalLab = "All subjects",
		stats = getStats("median\n(range)"), statsGeneralLab = "",
		rowAutoMerge = FALSE,
		labelVars = labelVars,
		title = toTitleCase("Table: Baseline Disease Characteristics (safety analysis set)"),
		file = file.path("tables_CSR", "Table_BaselineCharacteristics.docx")
	)


## ----table-MH-----------------------------------------------------------------------------------------------------------------------------------------------------------

	dataCM <- subset(dataAll$ADCM, SAFFL == "Y")

	# sort variable according to corresponding numeric variables
	dataCM$TRTA <- with(dataCM, reorder(TRTA, TRTAN))
	
	# Terms should be in lower-case
	dataCM$CMDECOD <- simpleCap(tolower(dataCM$CMDECOD))
	dataCM$CMCLAS <- simpleCap(tolower(dataCM$CMCLAS))
			
	getSummaryStatisticsTable(
		data = dataCM,
		colVar = "TRTA", colTotalInclude = TRUE, colTotalLab = "All subjects",
		rowVar = c("CMCLAS", "CMDECOD"), 
		# include total across generic terms and across ATC4 classes
		rowVarTotalInclude = c("CMCLAS", "CMDECOD"), 
		rowTotalLab = "Any prior and concomitant medication",
		stats = getStats("n (%)"),
		# sort rows based on counts of subjects in the total column 
		rowOrder = "total",
		labelVars = labelVars,
		emptyValue = 0,
		title = toTitleCase(paste("Prior and concomitant therapies",
			"by medication class and generic term (safety analyis set)"
		)),
		file = file.path("tables_CSR", "Table_CM.docx")
	)
	

## ----table-efficacy-----------------------------------------------------------------------------------------------------------------------------------------------------

	dataAdasCog11 <- subset(dataAll$ADQSADAS, PARAMCD == "ACTOT")
	dataCIBIC <- subset(dataAll$ADQSCIBC, PARAMCD == "CIBICVAL")
	
	dataEfficacy <- plyr::rbind.fill(dataAdasCog11, dataCIBIC)
	
	dataEfficacy$TRTP <- with(dataEfficacy, reorder(TRTP, TRTPN))
	dataEfficacy$AVISIT <- with(dataEfficacy, reorder(AVISIT, AVISITN))
	
	stats <- getStatsData(
		data = dataEfficacy, 
		var = c("AVAL", "CHG"), 
		type = c("n", "mean (se)", "median (range)")
	)
	
	getSummaryStatisticsTable(
		data = dataEfficacy,
		rowVar = "PARAM",
		colVar = c("TRTP", "AVISIT"),
		var = c("AVAL", "CHG"), 
		stats = stats,
		labelVars = labelVars,
		title = paste("Table: efficacy endpoints", 
			toTitleCase("actual value and changes from baseline per time point"				
		)),
		file = file.path("tables_CSR", "Table_efficacy.docx")
	)
	

## ----table-summaryTable-------------------------------------------------------------------------------------------------------------------------------------------------

	## data of interest: safety analysis set and treatment-emergent
	dataTEAE <- subset(dataAll$ADAE, SAFFL == "Y" & TRTEMFL == "Y")
	
	# order treatment and severity categories
	dataTEAE$TRTA <- with(dataTEAE, reorder(TRTA, TRTAN))
	
	## data considered for the total
	dataTotalAE <- subset(dataAll$ADSL, SAFFL == "Y")
	dataTotalAE$TRTA <- with(dataTotalAE, reorder(TRT01A, TRT01AN))
	
	# TEAE with worst intensity
	# build worst-case scenario
	dataTEAE$AESEV <- factor(dataTEAE$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
	dataTEAE$AESEVN <- as.numeric(dataTEAE$AESEV)
	dataTEAE <- ddply(dataTEAE, c("USUBJID", "TRTA"), function(x)
		cbind.data.frame(x, 
			WORSTINT = with(x, ifelse(AESEVN == max(AESEVN), as.character(AESEV), NA_character_))
	))
	dataTEAE$WORSTINT <- factor(dataTEAE$WORSTINT, levels = levels(dataTEAE$AESEV))
	
	## specify labels for each variable:
	varsAE <- c("TRTEMFL", "AESER", "AESDTH", "AEREL")
	
	# create the table
	getSummaryStatisticsTable(
		data = dataTEAE,
		colVar = "TRTA",
		# define variables to compute statistics on
		var = c("TRTEMFL", "AESER", "WORSTINT", "AESDTH", "AEREL"), 
		varFlag = c("TRTEMFL", "AESER", "AESDTH"),
		varLab = c(TRTEMFL = "Treatment-Emergent", WORSTINT = "Worst-case severity:"),
		varGeneralLab = "Subjects with, n(%):",
		# force the inclusion of lines for variable without count:
		varInclude0 = TRUE,
		# include the total for the worst-case scenario
		varTotalInclude = "WORSTINT",
		# statistics:
		stats = getStats('n (%)'),
		emptyValue = "0",
		labelVars = labelVars,
		# dataset used for the total in the header column (and for percentage as default)
		dataTotal = dataTotalAE,
		# title/export
		title = toTitleCase("Table: Summary Table of Treatment-emergent Adverse Events (safety analysis set)"),
		file = file.path("tables_CSR", "Table_TEAE_summary.docx")
	)
	

## ----table-TEAE---------------------------------------------------------------------------------------------------------------------------------------------------------

	dataTEAE <- subset(dataAll$ADAE, SAFFL == "Y" & TRTEMFL == "Y")
	
	# order treatment and severity categories
	dataTEAE$TRTA <- with(dataTEAE, reorder(TRTA, TRTAN))
	
	## data considered for the total
	dataTotalAE <- subset(dataAll$ADSL, SAFFL == "Y")
	dataTotalAE$TRTA <- with(dataTotalAE, reorder(TRT01A, TRT01AN))
	
	getSummaryStatisticsTable(
		data = dataTEAE,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		## total
		# data
		dataTotal = dataTotalAE,
		# row total
		rowVarTotalInclude = c("AESOC", "AEDECOD"), rowTotalLab = "Any TEAE",
		stats = getStats("n (%)"),
		labelVars = labelVars,
		rowVarLab = c('AESOC' = "TEAE by SOC and Preferred Term,\nn (%)"),
		# sort rows based on the total column:
		rowOrder = "total", 
		rowOrderTotalFilterFct = function(x) subset(x, TRTA == "Total"),
		title = paste("Table: Treatment-emergent Adverse Events by System Organ Class",
			"and Preferred Term (Safety Analysis Set)"
		),
		file = file.path("tables_CSR", "Table_TEAE_SOCPT_atLeast1Subject.docx")
	)


## ----table-TEAE-inAtLeast25Percent--------------------------------------------------------------------------------------------------------------------------------------

	getSummaryStatisticsTable(
		data = dataTEAE,
		rowVar = c("AESOC", "AEDECOD"),
		colVar = "TRTA",
		## total
		# data
		dataTotal = dataTotalAE, 
		# row total
		rowVarTotalInclude = c("AESOC", "AEDECOD"), rowTotalLab = "Any TEAE",
		stats = getStats("n (%)"),
		labelVars = labelVars,
		rowVarLab = c('AESOC' = "SOC and Preferred Term,\nn (%)"),
		# sort rows based on the total column:
		rowOrder = "total", 
		rowOrderTotalFilterFct = function(x) subset(x, TRTA == "Total"),
		title = paste("Table: Treatment-emergent Adverse Events by System Organ Class",
			"and Preferred Term reported in at least 25% of the subjects",
			"in any treatment group (Safety Analysis Set)"
		),
		file = file.path("tables_CSR", "Table_TEAE_SOCPT_atLeast25PercentsSubject.docx"),
		# include only events occuring in at least 25% for at least one preferred term:
		filterFct = function(x)
			ddply(x, "AESOC", function(x){ # per AESOC to include the total
				ddply(x, "AEDECOD", function(y){
					yTotal <- subset(y, grepl("Total", TRTA))
					if(any(yTotal$statPercN >= 25))	y
				})
			})
	)


## ----tableTEAE-worstCase------------------------------------------------------------------------------------------------------------------------------------------------

	dataTEAE <- subset(dataAll$ADAE, SAFFL == "Y" & TRTEMFL == "Y")
	
	# order treatment and severity categories
	dataTEAE$TRTA <- with(dataTEAE, reorder(TRTA, TRTAN))
	
	## data considered for the total
	dataTotalAE <- subset(dataAll$ADSL, SAFFL == "Y")
	dataTotalAE$TRTA <- with(dataTotalAE, reorder(TRT01A, TRT01AN))
	
	# TEAE with worst intensity
	dataTEAE$AESEV <- factor(dataTEAE$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
	dataTEAE$AESEVN <- as.numeric(dataTEAE$AESEV)
	
	# extract worst-case scenario data (only one record if multiple with same severity)
	dataAEWC <- ddply(dataTEAE, c("AESOC", "AEDECOD", "USUBJID", "TRTA"), function(x){
		x[which.max(x$AESEVN), ]
	})
	# worst-case scenario in lower case
	dataAEWC$WORSTINT <- simpleCap(tolower(dataAEWC$AESEV))
	labelVars["WORSTINT"] <- "Worst-case scenario"

	## datasets used for the total: 
	# for total: compute worst-case across SOC and across AE term
	# (otherwise patient counted in multiple categories if present different categories for different AEs)
	dataTotalRow <- list(
		# within SOC (across AEDECOD)
		'AEDECOD' = ddply(dataAEWC, c("AESOC", "USUBJID", "TRTA"), function(x){	
			x[which.max(x$AESEVN), ]
		}),
		# across SOC
		'AESOC' = ddply(dataAEWC, c("USUBJID", "TRTA"), function(x){	
			x[which.max(x$AESEVN), ]
		})
	)
	
	getSummaryStatisticsTable(
		data = dataAEWC,
		## row variables:
		rowVar = c("AESOC", "AEDECOD", "WORSTINT"), rowVarInSepCol = "WORSTINT",
		# include total across SOC and across AEDECOD
		rowVarTotalInclude = c("AESOC", "AEDECOD"), dataTotalRow = dataTotalRow, 
		rowVarTotalByVar = "WORSTINT", # count for each severity category for the total
		rowTotalLab = "Any TEAE", rowVarLab = c(AESOC = "Subjects with, n(%):", WORSTINT = "Worst-case scenario"),
		# sort per total in the total column
		rowOrder = "total", 
		## column variables
		colVar = "TRTA", 
		stats = getStats("n (%)"),
		emptyValue = "0",
		labelVars = labelVars,
		dataTotal = dataTotalAE,
		title = toTitleCase(paste("Table: Treatment-emergent Adverse",
			"Events by system organ",
			"and preferred term by worst-case (safety Analysis Set)"
		)),
		file = file.path("tables_CSR", "Table_TEAE_Severity.docx")
	)
	

## ----tableLab-----------------------------------------------------------------------------------------------------------------------------------------------------------

	dataLBAbn <- subset(dataAll$ADLBC, SAFFL == "Y" & LBNRIND != "NORMAL")
	
	dataLBAbn$PARAM <- with(dataLBAbn, reorder(PARAM, PARAMN))
	dataLBAbn$TRTA <- with(dataLBAbn, reorder(TRTA, TRTAN))
	dataLBAbn$LBNRIND <- factor(dataLBAbn$LBNRIND, levels = c("LOW", "HIGH"))

	dataLBAbnTotal <- subset(dataAll$ADSL, SAFFL == "Y")
	dataLBAbnTotal$TRTA <- with(dataLBAbnTotal, reorder(TRT01A, TRT01AN))
	
	getSummaryStatisticsTable(
		data = dataLBAbn,
		rowVar = c("PARCAT1", "PARAM"), 
		rowVarTotalInclude = c("PARCAT1", "PARAM"),
		colVar = "TRTA", 
		var = "LBNRIND", 
		rowVarInSepCol = "variableGroup", varSubgroupLab = "Abnormality",
		rowVarLab = c('PARCAT1' = "Laboratory Parameter\nn (%)"),
		stats = getStats("n (%)"),
		labelVars = labelVars,
		rowOrder = c("PARCAT1" = "total", "PARAM" = "total", "variableGroup" = "auto"),
		dataTotal = dataLBAbnTotal, 
		title = toTitleCase(paste("Table: Treatment-emergent",
			"Worst-case Laboratory Abnormalities (safety analysis set)"
		)),
		emptyValue = "0",
		file = file.path("tables_CSR", "Table_Lab_Severity.docx")
	)


## ----ECG-formatData, eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------
# 
# 	# data of interest
# 	paramsECG <- c("QT", "QTCF", "QRS", "PR", "RR", "EGHR")
# 
# 	dataECG <- subset(dataAll$ADEG, SAFFL == "Y" & PARAMCD %in% paramsECG)
# 	dataECG$TRTA <- with(dataECG, reorder(TRTA, TRTAN))
# 	dataECG$PARAM <- with(dataECG, reorder(PARAM, PARAMN))
# 	
# 	# consider all non-missing post-baseline records
# 	dataECGPostBaseline <- subset(dataECG,
# 		AVISIT %in% c("Screening", "Baseline", "Worst-case post-baseline")
# 	)
# 	
# 	# worst-case scenario:
# 	dataECGWC <- subset(dataECG, AVISIT == "Worst-case post-baseline")
# 	# treatment-emergent
# 	dataECGWC$TRTEMFL <- with(dataECGWC, ifelse(BASECAT1 != CHGCAT1, "Y", "N"))
# 	dataECGWCTE <- subset(dataECGWC, TRTEMFL == "Y")
# 	dataECGWC <- convertVarToFactor(dataECGWC,
# 		var = c("AVALCAT1", "CHGCAT1"),
# 		varNum = c("AVALCA1N", "CHGCAT1N")
# 	)
# 	
# 	# create the table
# 	getSummaryStatisticsTable(
# 		data = dataECGWC,
# 		# layout:
# 		colVar = "TRTA",
# 		rowVar = "PARAM", rowVarLab = c('PARAM' = "ECG Parameter"),
# 		# metrics to compute statistics on
# 		var = c("AVALCAT1", "CHGCAT1"),
# 		# in a separated column
# 		rowVarInSepCol = c("variable", "variableGroup"),
# 		# labels
# 		varGeneralLab = "Abnormality",
# 		varSubgroupLab = "Worst-Case Post-Baseline",
# 		stats = getStats("n (%)"),
# 		labelVars = labelVars,
# 		# total: all post-baseline
# 		dataTotal = dataECGPostBaseline,
# 		emptyValue = "0",
# 		rowVarTotalPerc = "PARAM", # total per parameter
# 		# ensure that categories are below the type of abnormality
# 		rowAutoMerge = FALSE,
# 		# only retain abnormalities:
# 		filterFct = function(x){
# 			subset(x, !variableGroup %in% c("<= 450 msec", "<= 30 msec"))
# 		},
# 		title = toTitleCase(paste("Table: Treatment-emergent worst-case",
# 			"ECG abnormalities and change from baseline ECG abnormalities (safety analysis set)"
# 		)),
# 		file = file.path("tables_CSR", "Table_ECG.docx")
# 	)
# 	

## ----tableVitalSigns----------------------------------------------------------------------------------------------------------------------------------------------------

	# analyis set and parameters of interest
	dataVS <- subset(dataAll$ADVS, 
		SAFFL == "Y" & ANL01FL == "Y" & VISIT != "BASELINE"
	)
	
	dataVS$PARAM <- with(dataVS, reorder(PARAM, PARAMN))
	dataVS$ANRIND <- with(dataVS, reorder(PARAM, PARAMN))
	dataVS$TRTA <- with(dataVS, reorder(TRTA, TRTAN))
	dataVS$SHIFT1 <- with(dataVS, factor(ifelse(SHIFT1 == "", NA_character_, SHIFT1)))
			
	getSummaryStatisticsTable(
		data = dataVS,
		rowVar = "PARAM", 
		rowVarInSepCol = "variableGroup", 
		rowVarInclude0 = TRUE,
		colVar = "TRTA", 
		var = "SHIFT1", varTotalInclude = TRUE,
		emptyValue = 0,
		stats = getStats("n (%)"),
		rowVarTotalPerc = "PARAM",
		labelVars = labelVars,
		title = toTitleCase(paste("Table: Treatment-emergent Worst-case",
			"Vital Sign Abnormalities (Safety Analysis Set)"
		)),
		file = file.path("tables_CSR", "Table_VitalSigns_Severity.docx")
	)


## ----PK-----------------------------------------------------------------------------------------------------------------------------------------------------------------

	paramcdPK <- c("AUCINFO", "CMAX", "TMAX")
	dataPK <- subset(dataAll$ADPP, PKFL == "Y" & PARAMCD %in% paramcdPK)
	
	dataPK$PARCAT1 <- with(dataPK, reorder(PARCAT1, PARCAT1N))
	dataPK$PARAMCD <- with(dataPK, reorder(PARAMCD, PARAMN))
	dataPK$TRTA <- with(dataPK, reorder(TRTA, TRTAN))
	dataPK$PARAMCD <- with(dataPK, reorder(PARAMCD, PARAMN))
	
	# build pretty labels
	labelsPK <- c(
		AUCINFO = "AUC_{Inf,obs}\n(h*ng/mL)",
		CMAX = "C_{max}\n(ng/mL)",
		TMAX = "t_{max}\n(h)"
	)
	dataPK$PARAM <- factor(dataPK$PARAMCD, 
		levels = levels(dataPK$PARAMCD), 
		labels = labelsPK[levels(dataPK$PARAMCD)]
	)
	
	statsPK <- dlply(dataPK, "PARAM", function(dataParam){
		getStatsData(
			data = dataParam,
			var = "AVAL",
			type = "median\n(range)",
			includeName = FALSE
		)[[1]]
	})
	
	getSummaryStatisticsTable(
		data = dataPK,
		rowVar = c("PARCAT1", "PARAM"), colVar = "TRTA",
		var = "AVAL",
#		rowVarLab = c('PARCAT1' = "PK parameters"),
		stats = statsPK, statsVarBy = "PARAM",
		emptyValue = "-",
		title = toTitleCase("Table: Summary of PK parameters (pharmacokinetics analysis set)"),
		file = file.path("tables_CSR", "Table_PK_Parameters.docx"),
		labelVars = labelVars
	)


## ----includeSessionInfo, echo = FALSE, results = "asis"-----------------------------------------------------------------------------------------------------------------
print(sessionInfo())

