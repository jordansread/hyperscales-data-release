
extract_model_ids <- function(hypso_fl, meteo_fl){
  warning('this is a temporary function and should be replaced with an inventory of **modeled** lakes')
  
  hypsos <- readRDS(hypso_fl)
  meteos <- readRDS(meteo_fl)
  names(hypsos)[names(hypsos) %in% meteos$site_id]
}
  