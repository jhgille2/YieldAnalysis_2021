##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

calc_LSMeans_2020 <- function(AllPheno_2020) {
  
  ##### REMOVE THIS LINE ONCE TEST 1 CAS P/O IS INCLUDED #####
  AllPheno_2020 <- AllPheno_2020 %>% dplyr::filter(Test == "Jay Test 2")
  
  # Add a protein + oil column
  AllPheno_2020 <- AllPheno_2020 %>% 
    mutate(protein_plus_oil = protein_dry_basis + oil_dry_basis) %>%
    dplyr::filter(Rep != 4)
  
  # Make sure the Location, Genotype, and Rep columns are factors
  AllPheno_2020$Genotype <- as.factor(AllPheno_2020$Genotype)
  AllPheno_2020$Loc      <- as.factor(AllPheno_2020$Loc)
  AllPheno_2020$Rep      <- as.factor(AllPheno_2020$Rep)
  
  # Which traits do I want LSMeans for?
  MeasuredTraits <- c("agscore", "Yield", "HT", "SDWT", "SQ", "protein_dry_basis", "oil_dry_basis", "protein_plus_oil")
  
  # Split the data based on test
  SplitTests <- split(AllPheno_2020, AllPheno_2020$Test)
  
  # Make a list to hold the LSMeans for each test
  AllLSMeans_2020        <- vector("list", length = length(SplitTests))
  names(AllLSMeans_2020) <- names(SplitTests)
  
  # Now, for each dataset (Test), I want to calculate the marginal means for each of my measured traits.
  # I'm using the emmeans function from the emmeans package to do this.
  # To get the marginal means, I need to fit a linear model first. The model I'm fitting has the form...
  #
  # Trait = Genotype + Loc + Genotype:Loc + Rep:Loc
  #
  # The emmeans function then takes this model, and the 
  

  # Split the data into sets for each test
  Data_2020_split <- split(AllPheno_2020, AllPheno_2020$Test)
  
  # Initialize lists to hold all the marginal means/models for each test
  Allemmeans        <- AllMeans        <- AllModels        <- vector('list', length = length(Data_2020_split))
  names(Allemmeans) <- names(AllMeans) <- names(AllModels) <- names(Data_2020_split)
  
  # Fit a linear model for each trait/test combination and then calculate marginal means for 
  # genotype from each model
  for(i in 1:length(Data_2020_split)){
    
    # Initialize lists to hold linear models and marginal means for each trait within each test
    emmeanList        <- ModelList        <- LSMeanList        <- vector('list', length = length(MeasuredTraits))
    names(emmeanList) <- names(ModelList) <- names(LSMeanList) <- MeasuredTraits
    
    for(j in 1:length(MeasuredTraits)){
      
      # Fit a linear model for the current trait in the current test
      CurrentLM <- lm(as.formula(paste(MeasuredTraits[[j]],"~Loc*Genotype + Rep:Loc")), data = Data_2020_split[[i]])
      
      # Put this linear model in the list that holds the models for the current test
      ModelList[[j]]  <- CurrentLM
      
      # Calculate marginal means using the current model, and add columns to indicate the current 
      # test and trait
      Currentemmeans      <- suppressMessages(emmeans(CurrentLM, "Genotype")) # Messages need to be suppressed so that the code plays nice with drake,
      CurrentMeans        <- as.data.frame(Currentemmeans)                    # specifically the environment locking
      CurrentMeans$Test   <- names(Data_2020_split)[[i]]
      CurrentMeans$Traits <- MeasuredTraits[[j]]
      
      # And put this augmented dataset in the list that holds the marginal means for the current test
      LSMeanList[[j]] <- CurrentMeans
      
      # Put the emmean object in the its list
      emmeanList[[j]] <- Currentemmeans
    }
    
    # Bind each dataframe in the current LSMeanList into a single dataframe and then put this 
    AllMeans[[i]]      <- do.call(rbind, LSMeanList)
    AllMeans[[i]]$Year <- 2020
    
    # Also put the list of fitted models into the list that holds the models for each test
    AllModels[[i]] <- ModelList
    
    # And put the unconverted emmean objects in its own list, these can be used later for processing/visualization
    Allemmeans[[i]] <- emmeanList
  }
  
  AllMeans <- do.call(bind_rows, AllMeans)
  
  # Return a list that has both the marginal means and the fitted models
  return(list(MarginalMeans = AllMeans, FittedModels = AllModels, emmeanObjects = Allemmeans))

}
