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

#This just downloads the files for summary so we can download the basic package for the data
# dpID = "DP1.20093.001"
# site_code = "CUPE"
# year_month = "2020-12"
# package = "expanded"
# savepath = "~/GitHub/NEON-water-chemistry-grab-samples/dataFolder"
# 
# uri <- paste0("http://data.neonscience.org/api/v0/data/", dpID, "/", site_code, "/",
#               year_month, "?package=", package)
# data_info <- jsonlite::fromJSON(txt = uri)
# data_info <- data_info[["data"]]
# data_url <- data_info$files$url[grep(pattern = ".csv", data_info$files$name)]
# data_name <- data_info$files$name[grep(pattern = ".csv", data_info$files$name)]
# for(i in 1:length(data_name)){
#   downloader::download(data_url[i], paste0(savepath, "/", data_name[i]), mode="wb")
# }

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "GUIL",
    package = 'basic',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte

#Read in the summary data
ecoCoreSummary <- read.csv("~/GitHub/NEON-water-chemistry-grab-samples/dataFolder/NEON.EcoCore_CSU.swc_externalLabSummaryData.20220131T152422Z.csv")
fiuSummary <- read.csv("~/GitHub/NEON-water-chemistry-grab-samples/dataFolder/NEON.Florida_International_University.swc_externalLabSummaryData.20220131T152422Z.csv")
#Add in an earlier labSpecificStartDate for FIU since they just started returning data
fiuSummary$labSpecificStartDate <- "2019-01-01"

allSummary <- rbind(ecoCoreSummary,fiuSummary)
allSummary$labSpecificEndDate[is.na(allSummary$labSpecificEndDate)|allSummary$labSpecificEndDate == ""] <- format(Sys.Date(), format = "%Y-%m-%d")
allSummary$labSpecificEndDate <- as.POSIXct(allSummary$labSpecificEndDate, tz = "GMT")
allSummary$labSpecificStartDate <- as.POSIXct(allSummary$labSpecificStartDate, tz = "GMT")
externalSummaryData <- allSummary

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
