# load example data
data(ADaMDataPelican)
data(labelVarsADaMPelican)

dataAE <- ADaMDataPelican$ADAE
labelVars <- labelVarsADaMPelican

# extract a variable containing patients screened and patient completed
# by default, records with 'var' set to 'value' are extracted:
dataAEGroup <- combineVariables(
	data = dataAE,
	newVar = "AEGRP",
	paramsList = list(
		# for all screened patients
		list(var = "SCRNFL", value = "Y"),
		# only for completers patients
		list(var = "COMPLFL", value = "Y")
	)
)
head(dataAEGroup, 1)
tail(dataAEGroup, 1)
# only the selected events are extracted:
table(dataAE$SCRNFL)
table(dataAE$COMPLFL)
nrow(dataAEGroup)

# extract records based on a custom function:
dataAEGroup <- combineVariables(
	data = dataAE,
	newVar = "AEGRP",
	paramsList = list(
		# for high severity
		list(var = "AESEVN", value = 2, fctTest = ">=", labelExtra = "higher than 2")
	)
)
head(dataAEGroup)
# only the selected events are extracted:
table(dataAE$AESEVN >= 2)
nrow(dataAEGroup)

# extract records based on a more complex filtering criteria
# by specifying an expression of the column names:
dataAEGroup <- combineVariables(
	data = dataAE,
	newVar = "AEGRP",
	paramsList = list(
		# related and with high severity
		list(
			exprs = 'AREL == "Related" & AESEVN >= 2',
			label = "Related AE with severity score higher than 2"
		)
	)
)
head(dataAEGroup)
# only the selected events are extracted:
with(dataAE, sum(AESEVN >= 2 & AREL == "Related"))
nrow(dataAEGroup)

labelVars["AEGRP"] <- "Patient groups of interest"
