hull_sf <- readRDS('/Users/RASV5G/OneDrive - cchmc/airPb/data-raw/prediction_extent_hull_sf.rds')
ndvi <- readRDS('/Users/RASV5G/OneDrive - cchmc/airPb/data-raw/ndvi_trimmed.rds')
population_sf <- readRDS('/Users/RASV5G/OneDrive - cchmc/airPb/data-raw/population_trimmed_sf.rds')
bus_lines_sf <- readRDS('/Users/RASV5G/OneDrive - cchmc/airPb/data-raw/bus_lines_sf.rds')
landcover <- readRDS('/Users/RASV5G/OneDrive - cchmc/airPb/data-raw/landcover_raster.rds')
pb_rf_model <- readRDS('/Users/RASV5G/OneDrive - cchmc/airPb/data-raw/pb_lurf_model.rds')

locations <- hc_tract_cent_coords
locations$.row <- seq_len(nrow(locations))
d <-
  locations %>%
  dplyr::select(.row, lat, lon) %>%
  na.omit() %>%
  dplyr::group_by(lat, lon) %>%
  tidyr::nest(.rows = c(.row)) %>%
  sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  dplyr::mutate(unique_index = seq_len(nrow(.)))

loc_sf <- d %>%
  sf::st_transform(3735)

loc_sf$domain_check_sf <- sf::st_intersects(loc_sf, hull_sf, byid = TRUE, sparse = FALSE)
if (! all(loc_sf$domain_check_sf )){
  warning(sum(! loc_sf$domain_check_sf ),
          ' point(s) not within spatial model domain;',
          ' these will be removed from output')
  loc_sf <- loc_sf[loc_sf$domain_check_sf == TRUE, ]
  out <- dplyr::select(loc_sf, -domain_check_sf)
} else {
  out <- loc_sf
}

out$greenspace_1000 <- raster::extract(ndvi, loc_sf, buffer = 1000 / 0.3048006096, fun = mean)

# population.density_500_sf
buffer <- sf::st_buffer(loc_sf, dist = 500 / 0.3048006096, nQuadSegs=1000)
intersection <- suppressWarnings(sf::st_intersection(buffer, st_buffer(population_sf, 0))) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(pop = as.numeric(as.character(FXS001)),
                area = as.numeric(as.character(area)),
                pop_density = pop / area)
out$population.density_500 <- intersection %>%
  dplyr::group_by(unique_index) %>%
  dplyr::summarize(mean_pop_density = mean(pop_density, na.rm = TRUE)) %>%
  .$mean_pop_density

# lines.length_bus_900_sf
buffer <- sf::st_buffer(loc_sf, dist = 900 / 0.3048006096, nQuadSegs = 1000)
intersection <- purrr::map(1:nrow(buffer), ~suppressWarnings(sf::st_intersection(buffer[.x,], bus_lines_sf)))
lengths <- purrr::map(intersection, ~ifelse(nrow(.x) > 0 , sum(sf::st_length(.)) * 0.3048006096, 0))
out$lines.length_bus_900 <- unlist(lengths)

# pasture_800_sf
loc_sf <- purrr::map(1:nrow(loc_sf), ~sf::st_transform(loc_sf[.x,], sf::st_crs(landcover)))
out$pasture_800 <- unlist(purrr::map(loc_sf, ~raster::extract(landcover, ., buffer = 800) %>%
                                       purrr::map_dbl(~ table(.x) %>%
                                                        prop.table() %>%
                                                        .['81'] %>%
                                                        as.numeric() %>%
                                                        ifelse(is.na(.), 0, .))) )

# developed.open_1100_sf
out$developed.open_1100 <- unlist(purrr::map(loc_sf, ~raster::extract(landcover, ., buffer = 1100) %>%
                                               purrr::map_dbl(~ table(.x) %>%
                                                                prop.table() %>%
                                                                .['21'] %>%
                                                                as.numeric() %>%
                                                                ifelse(is.na(.), 0, .))))

# developed.med_400_sf
out$developed.med_400 <- unlist(purrr::map(loc_sf, ~raster::extract(landcover, ., buffer = 400) %>%
                                             purrr::map_dbl(~ table(.x) %>%
                                                              prop.table() %>%
                                                              .['23'] %>%
                                                              as.numeric() %>%
                                                              ifelse(is.na(.), 0, .))))

# developed.low_900_sf
out$developed.low_900 <- unlist(purrr::map(loc_sf, ~raster::extract(landcover, ., buffer = 900) %>%
                                             purrr::map_dbl(~ table(.x) %>%
                                                              prop.table() %>%
                                                              .['22'] %>%
                                                              as.numeric() %>%
                                                              ifelse(is.na(.), 0, .))))


# developed.high_1500_sf
out$developed.high_1500 <- unlist(purrr::map(loc_sf, ~raster::extract(landcover, ., buffer = 1500) %>%
                                               purrr::map_dbl(~ table(.x) %>%
                                                                prop.table() %>%
                                                                .['24'] %>%
                                                                as.numeric() %>%
                                                                ifelse(is.na(.), 0, .))))


out$airPb <- stats::predict(pb_rf_model, newdata = out %>%
                              dplyr::select(unique_index, greenspace_1000:developed.high_1500) %>%
                              sf::st_drop_geometry())

out <- out %>%
  tidyr::unnest(cols = c(.rows)) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(locations, out, by = '.row') %>%
  dplyr::select(-.row, -unique_index)


