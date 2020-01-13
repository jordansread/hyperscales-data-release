
extract_model_ids <- function(pb0_job_list, results_dir, dummy){
  warning('this is a temporary function and should be replaced with an inventory of **modeled** lakes')
  files_here <- dir(results_dir)
  job_list <- readRDS(pb0_job_list)
  # these are export files...
  job_files <- unlist(sapply(1:length(job_list), function(x) c(basename(job_list[[x]]$export_file))) %>% c())
  
  model_ids <- files_here[files_here %in% job_files] %>% stringr::str_remove('pb0_') %>% stringr::str_remove("_temperatures.feather")
  
}

create_metadata_file <- function(fileout, sites, table, lakes_sf, lat_lon_fl, meteo_fl, gnis_names_fl){
  sdf <- sf::st_transform(lakes_sf, 2811) %>% 
    mutate(perim = lwgeom::st_perimeter_2d(Shape), area = sf::st_area(Shape), circle_perim = 2*pi*sqrt(area/pi), SDF = perim/circle_perim) %>% 
    sf::st_drop_geometry() %>% select(site_id, SDF) 

  sites %>% inner_join((readRDS(lat_lon_fl)), by = 'site_id') %>% 
    inner_join(sdf, by = 'site_id') %>% 
    rename(centroid_lon = longitude, centroid_lat = latitude) %>% 
    inner_join(table, by = 'site_id') %>% 
    inner_join(readRDS(meteo_fl), by = 'site_id') %>% 
    inner_join((readRDS(gnis_names_fl)), by = 'site_id') %>% rename(lake_name = GNIS_Name, meteo_filename = meteo_fl) %>% 
    write_csv(fileout)
  
}
bundle_nml_files <- function(json_filename, lake_ids, nml_ind){
  
  
  prep_proj_dir <- paste(str_split(nml_ind, '/')[[1]][1:2], collapse = '/')
  nml_files <- file.path(prep_proj_dir, names(yaml.load_file(nml_ind)))
  out_list <- vector("list", length = length(lake_ids)) %>% setNames(lake_ids)
  for (id in names(out_list)){
    this_nml_file <- nml_files[basename(nml_files) == paste0(id, '.nml')]
    if (!file.exists(this_nml_file)){
      stop(this_nml_file, " doesn't exist")
    }
    nml <- read_nml(nml_file = this_nml_file) %>% unclass()
    out_list[[id]] <- nml
  }
  
  RJSONIO::toJSON(out_list, pretty = TRUE) %>% write(json_filename)
}

zip_nml_files <- function(zipfile, lake_ids, nml_ind){
  
  cd <- getwd()
  on.exit(setwd(cd))
  zippath <- file.path(getwd(), zipfile)
  
  prep_proj_dir <- paste(str_split(nml_ind, '/')[[1]][1:2], collapse = '/')
  
  nml_files <- file.path(prep_proj_dir, names(yaml.load_file(nml_ind)))
  
  setwd(unique(dirname(nml_files))[1])
  zip(zippath, files = basename(nml_files))
  setwd(cd)
}

group_meteo_fls <- function(meteo_dir, groups){
  
  # turn files into point locations
  # check group match with assign_group_id(points, polygons)
  # return data.frame with id and filename
  
  meteo_fls <- data.frame(files = dir(meteo_dir), stringsAsFactors = FALSE) %>% 
    filter(stringr::str_detect(files, "[0-9n]\\].csv")) %>% 
    mutate(x = stringr::str_extract(files, 'x\\[[0-9]+\\]') %>% str_remove('x\\[') %>% str_remove('\\]') %>% as.numeric(),
           y = stringr::str_extract(files, 'y\\[[0-9]+\\]') %>% str_remove('y\\[') %>% str_remove('\\]') %>% as.numeric()) %>% 
    left_join(suppressWarnings(st_centroid(create_ldas_grid()))) %>% rename(geometry = ldas_grid_sfc) %>% select(-x, -y) %>% 
    st_sf()
  
  grouped_df <- st_intersects(x = meteo_fls, y = groups) %>% as.data.frame() %>% rename(group_idx = col.id)
  
  meteo_fls %>% mutate(row.id = row_number()) %>% 
    inner_join(grouped_df) %>% mutate(group_id = groups$group_id[group_idx], meteo_filepath = file.path(meteo_dir, files)) %>% 
    select(meteo_filepath, group_id) %>% st_drop_geometry()
  
}

zip_meteo_groups <- function(outfile, grouped_meteo_fls){

  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- unique(grouped_meteo_fls$group_id)
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/inputs_', group, '.zip')
    these_files <- grouped_meteo_fls %>% filter(group_id == !!group) %>% pull(meteo_filepath)
    
    zippath <- file.path(getwd(), zipfile)
    
    meteo_dir <- dirname(these_files) %>% unique()
    
    setwd(meteo_dir)
    zip(zippath, files = basename(these_files))
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}
  
zip_prediction_groups <- function(outfile, model_dir, model_ids, site_groups){
  
  model_feathers <- data.frame(site_id = model_ids, stringsAsFactors = FALSE) %>% 
    mutate(model_file = sprintf('pb0_%s_temperatures.feather', site_id)) %>% 
    inner_join(site_groups, by = 'site_id') %>% select(-site_id)
  
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- unique(model_feathers$group_id)
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/predictions_', group, '.zip')
    these_files <- model_feathers %>% filter(group_id == !!group) %>% pull(model_file)
    
    zippath <- file.path(getwd(), zipfile)
    
    these_files <- purrr::map(these_files, function(x){
      fileout <- file.path(tempdir(), paste0(tools::file_path_sans_ext(x), '.csv'))
      feather::read_feather(file.path(model_dir, x)) %>%
        select(-ice, date = DateTime) %>%
        mutate(date = as.Date(lubridate::ceiling_date(date, 'days'))) %>%
        write_csv(path = fileout)
      fileout
    }) %>% purrr::reduce(c)
    
    setwd(tempdir())
    zip(zippath, files = basename(these_files))
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}

zip_ice_flags_groups <- function(outfile, model_dir, model_ids, site_groups){
  model_feathers <- data.frame(site_id = model_ids, stringsAsFactors = FALSE) %>% 
    mutate(model_file = sprintf('pb0_%s_temperatures.feather', site_id)) %>% 
    inner_join(site_groups, by = 'site_id') %>% select(-site_id)
  
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- unique(model_feathers$group_id)
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/ice_flags_', group, '.zip')
    these_files <- model_feathers %>% filter(group_id == !!group) %>% pull(model_file)
    
    zippath <- file.path(getwd(), zipfile)
    
    these_files <- purrr::map(these_files, function(x){
      fileout <- file.path(tempdir(), str_remove(paste0(tools::file_path_sans_ext(x), '_ice_flag.csv'), '_temperatures'))
      feather::read_feather(file.path(model_dir, x)) %>%
        select(date = DateTime, ice) %>%
        mutate(date = as.Date(lubridate::ceiling_date(date, 'days'))) %>%
        write_csv(path = fileout)
      fileout
    }) %>% purrr::reduce(c)
    
    setwd(tempdir())
    zip(zippath, files = basename(these_files))
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}

zip_temp_obs <- function(outfile, temp_feather){

  cd <- getwd()
  on.exit(setwd(cd))
  
  zippath <- file.path(getwd(), outfile)
  csv_file <- paste0(tools::file_path_sans_ext(basename(outfile)) ,'.csv')
  csv_path <- file.path(tempdir(), csv_file)
  
  feather::read_feather(temp_feather) %>% 
    write_csv(path = csv_path)
  
  setwd(dirname(csv_path))
  zip(zipfile = zippath, files = csv_file)
  setwd(cd)
  
}