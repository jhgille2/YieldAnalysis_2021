##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param NIR_Files
clean_2020_NIR <- function(NIR_Files) {
  
  # Read in each of the files to a dataframe
  NIRTables <- lapply(NIR_Files, read_excel)
  
  # "NIR files" is a list of the NIR exports. 
  # Define a function that can be applied to each list element to 
  # clean the data from the NIR export
  NIR_Table_Clean <- function(NIRTable){
    
    # The row where measurements start 
    MeasureRow <- which(NIRTable[, 1] == "Sample ID") + 1
    
    # The row where the header is kept
    HeaderRow <- NIRTable[which(NIRTable[, 1] == "Product:"), ]

    # Pull out just measurements
    Measurements <- NIRTable[MeasureRow:(nrow(NIRTable) - 1), ]
    
    # Bind the header to the measurements and then use the first row (the header) as column names.
    AllData <- bind_rows(HeaderRow, Measurements) %>% 
      row_to_names(1) %>%
      clean_names() %>%
      # Rename columns then only keep the code, date, protein and oil measurements
      rename(NIR_Code = product,
             Date     = x2019_a_whole_small) %>%
      select(NIR_Code, 
             Date,
             protein_dry_basis,
             oil_dry_basis) %>%
      # Split the NIR code into its components
      separate(NIR_Code,
               into = c("Year", "Loc", "Test", "Genotype", "Code", "Plot", "Rep", "NIR_Number"),
               sep = "_") %>%
      # Convert the date column to a proper date
      mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

    AllData
  }
  
  # Apply this function to each NIR export, merge all files, and return the result
  Cleaned_NIR <- lapply(NIRTables, NIR_Table_Clean)
  do.call(bind_rows, Cleaned_NIR) %>% mutate(Year              = as.numeric(Year), 
                                             Rep               = as.numeric(Rep),
                                             Code              = as.numeric(Code),
                                             Plot              = as.numeric(Plot),
                                             protein_dry_basis = as.numeric(protein_dry_basis),
                                             oil_dry_basis     = as.numeric(oil_dry_basis),
                                             Loc               = toupper(Loc))
}
