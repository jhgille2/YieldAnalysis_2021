##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

load_DriveFiles <- function() {

  # The drive links for each field book
  Test1Cla <- "https://docs.google.com/spreadsheets/d/1HoUM7hzo9f60M3M92bHEKeKs4AgAoch9grD98l24jBw/edit?usp=sharing"
  Test2Cla <- "https://docs.google.com/spreadsheets/d/1S8qjptGXF8-08w2Uv2CCyZyRqMx4kEUg4AJ5hg6YaPs/edit?usp=sharing"
  
  AllLinks <- c(Test1Cla, Test2Cla)
  names(AllLinks) <- c("Test1Cla", "Test2Cla")
  
  # A list to hold the dataframes
  AllTestData <- vector("list", length = length(AllLinks))
  names(AllTestData) <- names(AllLinks)
  
  # Read each table from google drive to the list
  for(i in seq_along(AllLinks)){
    AllTestData[[i]] <- gsheet2tbl(AllLinks[[i]])
  }

  # And return the list
  AllTestData <- do.call(rbind, AllTestData)
  AllTestData
}
