##############################################################################################
#' @title This script links external summary data with external lab data

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function adds the following columns to the externalLabByAnalyte table: 
#' "instrument", 
#' "method", 
#' "methodModification",
#' "methodDetectionLimit",
#' "precision",
#' "precisionRepeatabilityUnits",
#' "absorbancePrecision",
#' "measurementUncertainty",
#' "measurementUncertaintyUnits",
#' "internalLabName",
#' "labSpecificStartDate",
#' "labSpecificEndDate"

#' @references
#' License: Creative Commons Zero v1.0 Universal

#' @keywords surface water, streams, rivers, water chemistry, grab sample


# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2022-01-27)
#     original creation
##############################################################################################

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "CRAM",
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <-
  as.POSIXct(Sys.Date())

#Check that the analytes match
externalLabData$analyte[!externalLabData$analyte %in% externalSummaryData$analyte]

#Link the data by analyte
namesToAdd <-
  names(externalSummaryData)[!(names(externalSummaryData) %in% names(externalLabData))]
namesToAdd <- namesToAdd[!(namesToAdd %in% c("endDate"))]
externalLabData[, namesToAdd] <- NA
for (i in 1:nrow(externalSummaryData)) {
  currStartDate <- externalSummaryData$labSpecificStartDate[i]
  currEndDate <- externalSummaryData$labSpecificEndDate[i]
  currAnalyte <- externalSummaryData$analyte[i]
  currLab <- externalSummaryData$laboratoryName[i]
  externalLabData[(
    externalLabData$startDate >= currStartDate &
      externalLabData$startDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}