## code to prepare `DATASET` dataset goes here
daily <- dataRetrieval::readNWISdv(
  siteNumbers = "01481500", 
  parameterCd = "00060", 
  startDate = "2007-10-01", 
  endDate = "2023-09-30"
)

sample <- dataRetrieval::readNWISdv(
  siteNumbers = "01481500", 
  parameterCd = "00095", 
  startDate = "2007-10-01", 
  endDate = "2023-09-30"
)

daily <- daily[, -c(1,2,5)]
colnames(daily) <- c("date", "q")

sample <- sample[, -c(1,2,5)]
colnames(sample) <- c("date", "sc")

streamdat <- merge(daily, sample, by = "date", all.x = TRUE, all.y = TRUE)

usethis::use_data(streamdat, overwrite = TRUE)
