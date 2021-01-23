##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param LSMeans_2019_clean
##' @param LSMeans_2020_reduced
CombineMeans <- function(LSMeans_2019_clean, LSMeans_2020_reduced) {

  LSMeans_2020_reduced <- LSMeans_2020_reduced %>% select(-one_of("Test"))
  AllMeans <- bind_rows(LSMeans_2019_clean, LSMeans_2020_reduced)
  
  # Get just the protein, oil, and yield data
  ProtOilYield <- AllMeans %>%
    dplyr::filter(Traits %in% c("oil_dry_basis", "protein_dry_basis", "Yield"))
  
  AllMeans

}
