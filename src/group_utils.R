
generate_lat_lon_groups <- function(x0, y0, x_num, y_num, cell_res){
  ldas_crs <- "+init=epsg:4326"
  
  ldas_grid_sfc <- sf::st_make_grid(cellsize = cell_res, n = c(x_num, y_num),
                                    offset = c(x0-cell_res/2, y0-cell_res/2), crs = ldas_crs)
  # cells count left to right, then next row, then left to right
  
  groups <- data.frame(group_id = rep(NA_character_, length(ldas_grid_sfc)), stringsAsFactors = FALSE)
  for (i in 1:length(ldas_grid_sfc)){
    # format of N46.125-48.625_W86.5-89.25
    this_box <- st_bbox(ldas_grid_sfc[i])
    groups$group_id[i] <- sprintf("N%1.2f-%1.2f_W%1.2f-%1.2f", this_box$ymin, this_box$ymax, -this_box$xmax, -this_box$xmin)
  }
  
  ldas_grid <- st_sf(groups, ldas_grid_sfc)
  return(ldas_grid)
}


assign_group_id <- function(points, polygons){
  points %>% mutate(group_id = {st_intersects(x = points, y = polygons) %>% unlist %>% polygons$group_id[.]}) %>% 
    st_drop_geometry()
}