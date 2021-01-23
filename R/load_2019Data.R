##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

load_2019Data <- function(Data_2019, mergeChecks = FALSE) {

  # The names of the checks in this data
  Checks <- c("Dunphy", "Dilday", "Roy", "Osage", "N.C. Raleigh")
  
  # And the population parents
  Parents <- c("LMN09-119", "N09-09", "LMN09-19", "N13-47")
  
  # Convert some columns to factors
  FactorColumns            <- c("Loc", "Test", "Year", "Rep", "Code", "Plot", "Genotype")
  Data_2019[FactorColumns] <- lapply(Data_2019[FactorColumns], as.factor)
  
  # The traits I want LSMeans for
  MeasuredTraits <- c("Oil",
                      "Protein",
                      "Yield",
                      "HT",
                      "SDWT",
                      "Protein_Plus_Oil",
                      "AgScore",
                      "LOD")
  
  
  # Split the data into sets for each test
  Data_2019_split <- split(Data_2019, Data_2019$Test)
  
  # Initialize lists to hold all the marginal means/models for each test
  Allemmeans        <- AllMeans        <- AllModels        <- vector('list', length = length(Data_2019_split))
  names(Allemmeans) <- names(AllMeans) <- names(AllModels) <- names(Data_2019_split)
  
  # Fit a linear model for each trait/test combination and then calculate marginal means for 
  # genotype from each model
  for(i in 1:length(Data_2019_split)){
    
    # Initialize lists to hold linear models and marginal means for each trait within each test
    emmeanList        <- ModelList        <- LSMeanList        <- vector('list', length = length(MeasuredTraits))
    names(emmeanList) <- names(ModelList) <- names(LSMeanList) <- MeasuredTraits
    
    for(j in 1:length(MeasuredTraits)){
      
      # Fit a linear model for the current trait in the current test
      CurrentLM <- lm(as.formula(paste(MeasuredTraits[[j]],"~Loc*Genotype + Rep:Loc")), data = Data_2019_split[[i]])
      
      # Put this linear model in the list that holds the models for the current test
      ModelList[[j]]  <- CurrentLM
      
      # Calculate marginal means using the current model, and add columns to indicate the current 
      # test and trait
      Currentemmeans      <- suppressMessages(emmeans(CurrentLM, "Genotype")) # Messages need to be suppressed so that the code plays nice with drake,
      CurrentMeans        <- as.data.frame(Currentemmeans)                    # specifically the environment locking
      CurrentMeans$Test   <- names(Data_2019_split)[[i]]
      CurrentMeans$Traits <- MeasuredTraits[[j]]
      
      # And put this augmented dataset in the list that holds the marginal means for the current test
      LSMeanList[[j]] <- CurrentMeans
      
      # Put the emmean object in the its list
      emmeanList[[j]] <- Currentemmeans
    }
    
    # Bind each dataframe in the current LSMeanList into a single dataframe and then put this 
    AllMeans[[i]]  <- do.call(rbind, LSMeanList)
    
    # Also put the list of fitted models into the list that holds the models for each test
    AllModels[[i]] <- ModelList
    
    # And put the unconverted emmean objects in its own list, these can be used later for processing/visualization
    Allemmeans[[i]] <- emmeanList
  }
  
  # Return a list that has both the marginal means and the fitted models
  return(list(MarginalMeans = AllMeans, FittedModels = AllModels, emmeanObjects = Allemmeans))
}
