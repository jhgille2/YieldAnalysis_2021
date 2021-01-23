

# The address of the 2019 yield data
YieldData_2019  <- paste0(here(), "/Data/LSMeans_Jay_Yield_2019.csv")
WeightData_2020 <- paste0(here(), "/Data/WeightData_JayYield_2020.xlsx")
MetanDest       <- "C:\\Users\\Jay\\Desktop\\Work\\Projects\\Yield_2020_Metan\\Data\\YieldData_2020.csv"

# The NIR File addresses
NIR_Files <- list.files(paste0(here(), "/Data/NIR/"), full.names = TRUE)

the_plan <-
  drake_plan(
    
    # Load in the field notes from google drive, clean it, and calculate marginal means
    DriveData_2020    = load_DriveFiles(),               # Read data from the notes kept on google drive (Clayton field notes)
    Cleaned_Data_2020 = clean_2020_Data(DriveData_2020), # Clean the notes from google drive 
    
    # Read in and clean NIR data
    NIRData = clean_2020_NIR(NIR_Files),
    
    # Add yield and seed weight measurements to the 2020 yield data
    AllWeightData_2020 = add_Weights(Cleaned_Data_2020, WeightData_2020),
    
    # Add protein and oil measurements to the 2020 yield data
    AllPheno_2020 = left_join(AllWeightData_2020$CompleteData, NIRData) %>% 
      mutate(SDWT = as.numeric(SDWT)),
    
    # Save this file to the metan directory
    Yield_2020__Outfile = write.csv(AllPheno_2020, file_out(!!MetanDest), row.names = F),
    
    # Calc LSMeans for 2020
    ## FILTERINGG FOR TEST 2 ONLY RIGHT NOW ##
    LSMeans_2020 = calc_LSMeans_2020(AllPheno_2020), 
   
     # A smaller dataset with only the marginal means, 
    LSMeans_2020_reduced = LSMeans_2020$MarginalMeans %>% select(Genotype,
                                                                 Test, 
                                                                 emmean,
                                                                 Traits, 
                                                                 Year),
    
    # Load in the LSMeans from 2019, clean column names, and filter to just the 
    # genotypes included in 2020.
    LSMeans_2019         = read.csv(file_in(!!YieldData_2019)),
    LSMeans_2019_clean   = clean_2019Data(LSMeans_2019, Cleaned_Data_2020),
    
    # Combine 2020 LSMeans with 2019 LSMeans
    AllMeans = CombineMeans(LSMeans_2019_clean, LSMeans_2020_reduced),
    
    # An output table, remove checks that were only included in one year, and
    # show how values compare between 
    YearCompare = Compare_years(AllMeans)
    
    # MarMeans_2019     = load_2019Data(Data_2019),
    # 
    # LSMeans_2019      = MarMeans_2019$MarginalMeans,
    # FittedModels_2019 = MarMeans_2019$FittedModels,
    # emmeans_2019      = MarMeans_2019$emmeanObjects

   

)
