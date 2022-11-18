# load example data
library(clinUtils)
data(dataADaMCDISCP01)

dataSL <- dataADaMCDISCP01$ADSL
labelVars <- attr(dataADaMCDISCP01, "labelVars")

## Summary tables can be combined next to each other
# for example to combine summary tables from multiple
# studies with different compounds:
varsSL <- c("HEIGHTBL", "WEIGHTBL", "BMIBL")

# study with low dose of the compound
summaryTableXanoLow <- computeSummaryStatisticsTable(
	data = subset(dataSL, TRT01P == "Xanomeline Low Dose"),
	var = varsSL,
	stats = getStats(c("n", "median (range)")),
	labelVars = labelVars
)
# study with high dose of the compound
summaryTableXanoHigh <- computeSummaryStatisticsTable(
	data = subset(dataSL, TRT01P == "Xanomeline High Dose"),
	var = varsSL,
	stats = getStats(c("n", "median (range)")),
	labelVars = labelVars
)
summaryTable <- combine(
	`Low dose` = summaryTableXanoLow, 
	`High dose` = summaryTableXanoHigh,
	combineVar = "Treatment",
	combineDir = "col"
)
export(summaryTable)

## Summary tables with different set statistics
# and variables can be combined as well
# This can specify the specification of the parameters

# demography table with categorical variables
summaryTableCat <- computeSummaryStatisticsTable(
	data = dataSL,
	var = c("HEIGHTBL", "WEIGHTBL", "BMIBL", "AGE"),
	colVar = "TRT01P",
	stats = getStats(c("n", "median (range)")),
	labelVars = labelVars
)
# demography table with continuous variables
summaryTableCont <- computeSummaryStatisticsTable(
	data = dataSL,
	var = c("SEX", "RACE", "ETHNIC"),
	colVar = "TRT01P",
	stats = getStats("n (%)", includeName = FALSE),
	varInclude0 = TRUE,
	labelVars = labelVars
)
summaryTable <- combine(summaryTableCat, summaryTableCont)
export(summaryTable)
