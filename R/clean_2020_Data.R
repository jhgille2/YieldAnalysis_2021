##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

clean_2020_Data <- function(AllData_2020) {

  # Convert the location, Test, Year, Rep, Code, Plot
  # Lodging, and Genotype columns to factors
  
# FactorColumns <- c("Loc", "Test", "Year", "Rep", "Code", "Plot", "LOD", "Genotype")
# AllData_2020[FactorColumns] <- lapply(AllData_2020[FactorColumns], as.factor)

# Remove the last two columns of the data, at the time of writing (2020/10/30), these two columns contain no
# data. This may change in the future as protein and oil measurements are taken. To help prevent against an
# arbitrary deletion of the last two columns if additional columns are added in the future, I have hard-coded
# the names of the columns to be removed here.
AllData_2020 <- AllData_2020 %>% select(-one_of("Note1", "Yield", "SDWT"))

AllData_2020
}
