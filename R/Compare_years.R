##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param AllMeans
Compare_years <- function(AllMeans) {

  # Add a column to identify traits by year, pivot using this new column,
  # and select only protein, oil and yield traits
  AllMeans <- AllMeans %>%
    dplyr::filter(Traits %in% c("oil_dry_basis", "protein_dry_basis", "Yield", "protein_plus_oil")) %>%
    mutate(Trait_Year = paste(Traits, Year, sep = "_")) %>%
    pivot_wider(id_cols     = Genotype, 
                names_from  = Trait_Year, 
                values_from = emmean) %>%
    select(Genotype, 
           oil_dry_basis_2019,
           oil_dry_basis_2020,
           protein_dry_basis_2019,
           protein_dry_basis_2020,
           protein_plus_oil_2019,
           protein_plus_oil_2020,
           Yield_2019,
           Yield_2020)
  
  AllMeans
}
