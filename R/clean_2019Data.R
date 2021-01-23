##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param LSMeans_2019
clean_2019Data <- function(LSMeans_2019, Cleaned_Data_2020) {
  
  ##### REMOVE THIS LINE ONCE TEST 1 CAS P/O IS INCLUDED #####
  Cleaned_Data_2020 <- Cleaned_Data_2020 %>% dplyr::filter(Test == "Jay Test 2")
  
  # Filter to just the genotypes which are included in the 2020 trials
  LSMeans_Only2020 <- LSMeans_2019 %>%
    dplyr::filter(Genotype %in% Cleaned_Data_2020$Genotype) %>%    # Filter to only the genotypes that were grown in 2020
    #rename_at(vars(-Genotype), function(x) paste0(x, "_2019")) %>% # Rename columns so it is obvious which data come from 2019
    group_by(Genotype) %>%
    # Average repeated Genotypes (parents and checks)
    summarise_if(is.numeric, mean) %>%
    clean_names() %>%
    rename(Genotype         = genotype,
           agscore          = ag_score,
           HT               = ht,
           SDWT             = hundred_seed_weight, 
           protein_plus_oil = p_o,
           Yield            = bulk_weight) %>%
    mutate(Year = 2019) %>%
    pivot_longer(cols = 2:9) %>%
    rename(Traits = name, 
           emmean = value) %>%
    arrange(Traits, Genotype)

  LSMeans_Only2020
}
