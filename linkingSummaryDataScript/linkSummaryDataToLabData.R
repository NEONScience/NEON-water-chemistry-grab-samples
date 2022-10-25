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
#   Kaelin M. Cawley (2022-10-25)
#     update for LAGOS
##############################################################################################

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = c("LIRO", "CRAM","PRPO","PRLA","SUGG","BARC"),
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
# Add in now as the labSpecificEndDate for current records in the summary data
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <- Sys.Date()

# This data should be linked by analysisData in swc_externalLabDataByAnalyte and labSpecificStartDate and labSpecificEndDate in swc_externalLabSummaryData
# For older data the analysisDate wasn't populated, but we can assume it's about collectDate + 7 days
externalLabData$analysisDate[is.na(externalLabData$analysisDate)] <- externalLabData$collectDate[is.na(externalLabData$analysisDate)] + 7*24*60*60

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
    externalLabData$analysisDate >= currStartDate &
      externalLabData$analysisDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}

#Add in the water volume filtered and SIRFER estimated MDLs from https://www.neonscience.org/impact/observatory-blog/delay-external-lab-particulate-carbon-and-nitrogen-data
carbonSirferMDL <- 4 #micrograms
nitrogenSirferMDL <- 6 #micrograms
externalLabData$methodDetectionLimit[externalLabData$laboratoryName == "SIRFER Lab at University of Utah" & externalLabData$analyte == "TPC"] <- carbonSirferMDL
externalLabData$methodDetectionLimit[externalLabData$laboratoryName == "SIRFER Lab at University of Utah" & externalLabData$analyte == "TPN"] <- nitrogenSirferMDL

fieldData <- waterChemList$swc_asiPOMFieldData
swcFieldData <- waterChemList$swc_fieldData
externalLabData$sampleVolumeFiltered <- NA
externalLabData$mass <- NA
for(i in 1:nrow(externalLabData)){
  currSampleID <- externalLabData$sampleID[i]
  if(externalLabData$laboratoryName[i] == "SIRFER Lab at University of Utah"){
    try(externalLabData$sampleVolumeFiltered[i] <- fieldData$sampleVolumeFilteredPOMRep1[!is.na(fieldData$isotopePOMSampleID) & fieldData$isotopePOMSampleID == currSampleID], silent = TRUE)
    try(externalLabData$sampleVolumeFiltered[i] <- fieldData$sampleVolumeFilteredPOMRep2[!is.na(fieldData$isotopePOMRep2SampleID) & fieldData$isotopePOMRep2SampleID == currSampleID], silent = TRUE)
  }
  if(externalLabData$laboratoryName[i] == "EcoCore_CSU" & externalLabData$analyte[i] %in% c("TPC","TPN")){
    try(externalLabData$sampleVolumeFiltered[i] <- swcFieldData$sampleVolumeFiltered[!is.na(swcFieldData$pcnSampleID) & swcFieldData$pcnSampleID == currSampleID], silent = TRUE)
  }
}

externalLabData$mass[externalLabData$analyte %in% c("TPC","TPN")] <- externalLabData$analyteConcentration[externalLabData$analyte %in% c("TPC","TPN")] * (externalLabData$sampleVolumeFiltered[externalLabData$analyte %in% c("TPC","TPN")]/1000)
