
sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

subset_lake_sf <- function(lakes_sf_fl, site_ids){
  readRDS(lakes_sf_fl) %>% filter(site_id %in% !!site_ids) %>% sf::st_zm()
}


plot_grouped_preview <- function(fileout, spatial_groups, modeled_centroids_sf, site_ids_grouped, lakes_sf_fl){
  png(filename = fileout, width = 8, height = 5.5, units = 'in', res = 500)
  par(omi = c(0,0,0,0), mai = c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  
  n <- length(unique(spatial_groups$group_id))
  cols <- c('#a6cee3','#b2df8a','#1f78b4','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#b15928',
            '#e41a1c','#377eb8','#984ea3','#a65628','#f781bf','#007f7f','#ff00ff')
  
  #set.seed(41)
  col_vector <- rep(cols, ceiling(n/length(cols)))[1:n]#sample(cols, size = n, replace = TRUE) 
  g_styles <- data.frame(group_id = unique(spatial_groups$group_id), col = col_vector, stringsAsFactors = FALSE)
  
  spatial_groups <- left_join(spatial_groups, g_styles, by = 'group_id')
  
  plot(st_geometry(spatial_groups), col = paste0(spatial_groups$col, '4D'), border = 'grey70', lwd = 0.1, reset = FALSE)


  all_lakes_simple <- readRDS(lakes_sf_fl) %>% st_transform(crs = "+init=epsg:2811") %>% sf::st_simplify(dTolerance = 40) %>% 
    st_transform(crs = "+init=epsg:4326")
  
  plot(st_geometry(all_lakes_simple), col = 'grey70', border = 'grey70', lwd = 0.1, add = TRUE)
  
  plot(st_geometry(spatial_groups), col = paste0(spatial_groups$col, '4D'), border = 'grey70', lwd = 0.1, add = TRUE)
  
  modeled_centroids_sf <- inner_join(all_lakes_simple, site_ids_grouped, by = 'site_id') %>% 
    left_join(g_styles, by = 'group_id')
  
  plot(st_geometry(modeled_centroids_sf), col = 'dodgerblue', border = 'dodgerblue', lwd = 0.2, add = TRUE)
  #plot(st_centroid(st_geometry(modeled_centroids_sf)), col = modeled_centroids_sf$col, lwd = 0.2, add = TRUE, cex = 0.25)
  
  for (j in 1:nrow(spatial_groups)){
    bbox <- st_bbox(spatial_groups[j,])
    
    text(bbox[1], bbox[2]+0.1, str_extract(spatial_groups[j,]$group_id, '[0-9]{2}'), pos = 4, cex = 0.8, offset = 0.1)
  }
  
  dev.off()
  
}


