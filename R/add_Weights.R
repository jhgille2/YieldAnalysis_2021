##' This function adds yield, seed quality, and 100 seed weight measurements to the data collected from Clayton, and 
##' joins this data with the files from Caswell, which already have field and weight data in a single file. 
##'
##'
##' @title
##' @param Cleaned_Data_2020
##' @param WeightData_2020
##' @return A dataframe with all weight and field data for the Caswell and Clayton locations for the 2020 Jay Yield trial data. 
add_Weights <- function(Cleaned_Data_2020, WeightData_2020) {

  # Cleaned_Data_2020 has field notes from Clayton, but no yield, SQ, or SDWT 
  # measurements. WeightData are the files which hold the weight measurements for
  # the CLA trials, and all the data for the CAS trials. 

  # Load in the yield data
  # The spreadsheet has four sheets, one for each location/test combination
  ClaytonSheetNames <- c("Cla_Test1", "Cla_Test2") # The Clayton sheets
  CaswellSheetNames <- c("Cas_Test1", "Cas_Test2") # And the Caswell sheets
  
  # Lists to hold the dataframes for each location
  ClaytonData <- vector("list", length = length(ClaytonSheetNames))
  CaswellData <- vector("list", length = length(CaswellSheetNames))
  
  names(ClaytonData) <- ClaytonSheetNames
  names(CaswellData) <- CaswellSheetNames
  
  # Read in each sheet
  for(i in 1:length(excel_sheets(WeightData_2020))){
    CurrentSheetName <- excel_sheets(WeightData_2020)[[i]]
    # Processing for the Clayton data
    if(CurrentSheetName %in% ClaytonSheetNames){
      # Read in the current sheet
      ClaytonData[[CurrentSheetName]] <- read_excel(WeightData_2020, sheet = CurrentSheetName) %>%
        select(-one_of("MD", "PC", "LOD", "HT", "Note1"))
      
    }else if(CurrentSheetName %in% CaswellSheetNames){ # Read in the Caswell data
      CaswellData[[CurrentSheetName]] <- read_excel(WeightData_2020, sheet = CurrentSheetName) %>%
        select(-one_of("MD", "PC", "Note1", "FC"))
    }
  }
  
  # Merge the Clayton datasets and join with the phrnotype data
  ClaytonData <- do.call(bind_rows, ClaytonData) %>%
    left_join(Cleaned_Data_2020)
  
  # Merge the Caswell data
  CaswellData <- do.call(bind_rows,  CaswellData)
  
  # Merge the Clayton and Caswell data
  AllData <- bind_rows(ClaytonData, CaswellData) %>%
    mutate(Loc = toupper(Loc))
  
  # Make another dataset which keeps only the variables with complete measurements
  CompleteData <- AllData %>% select(-one_of("MD", "PC", "FC"))
  
  # Return the two data sets
  return(list(FullData = AllData, CompleteData = CompleteData))
}
