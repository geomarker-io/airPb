make_spatial_predictors <- function(loc = data.frame(lon = -84.517997, lat = 39.1502776)){
    out <- loc

    loc_sf <- loc %>%
        dplyr::filter(!is.na(lat), !is.na(lon)) %>%
        dplyr::mutate(old_lat = lat, old_lon = lon) %>%
        sf::st_as_sf(coords=c('lon', 'lat'), crs=4326) %>%
        sf::st_transform(3735) %>%
        dplyr::group_by(id, old_lon, old_lat) %>%
        tidyr::nest()

    # remove points not inside spatial model domain
    loc_sf$domain_check_sf <- purrr::map(loc_sf$data, ~sf::st_intersects(.x, hull_sf, byid = TRUE, sparse = FALSE)) %>%
        purrr::map(~ .x == 1) %>% unlist()
    if (! all(loc_sf$domain_check_sf )){
        warning(sum(! loc_sf$domain_check_sf ),
                ' point(s) not within spatial model domain;',
                ' these will be removed from output')
        loc_sf <- loc_sf[loc_sf$domain_check_sf == TRUE, ]
        loc_sf <- dplyr::select(loc_sf, -domain_check_sf)
        out <- out[out$lat %in% loc_sf$old_lat & out$lon %in% loc_sf$old_lon,]
    }

    out$greenspace_1000 <- unlist(purrr::map(loc_sf$data, ~raster::extract(ndvi, .x, buffer = 1000 / 0.3048006096, fun = mean)))

    # population.density_500_sf
    buffer <- purrr::map(loc_sf$data, ~sf::st_buffer(.x, dist = 500 / 0.3048006096, nQuadSegs=1000))
    intersection <- purrr::map(buffer, ~suppressWarnings(sf::st_intersection(.x, population_sf)) %>%
                            sf::st_drop_geometry() %>%
                            dplyr::mutate(pop = as.numeric(as.character(FXS001)),
                                   area = as.numeric(as.character(area)),
                                   pop_density = pop / area))
    out$population.density_500 <- unlist(purrr::map(intersection,
                                                   ~dplyr::summarize(.x, mean_pop_density = mean(pop_density, na.rm = TRUE))))


    # lines.length_bus_900_sf
    buffer <- purrr::map(loc_sf$data, ~sf::st_buffer(.x, dist = 900 / 0.3048006096, nQuadSegs = 1000))
    intersection <- purrr::map(buffer, ~suppressWarnings(sf::st_intersection(.x, bus_lines_sf)))
    lengths <- purrr::map(intersection, ~ifelse(nrow(.x) > 0 , sum(sf::st_length(.)) * 0.3048006096, 0))
    out$lines.length_bus_900 <- unlist(lengths)

    # pasture_800_sf
    loc_sf$data <- purrr::map(loc_sf$data, ~sf::st_transform(.x, sf::st_crs(landcover)))
    out$pasture_800 <- unlist(purrr::map(loc_sf$data, ~raster::extract(landcover, ., buffer = 800) %>%
                                            purrr::map_dbl(~ table(.x) %>%
                                                        prop.table() %>%
                                                        .['81'] %>%
                                                        as.numeric() %>%
                                                        ifelse(is.na(.), 0, .))) )

    # developed.open_1100_sf
    out$developed.open_1100 <- unlist(purrr::map(loc_sf$data, ~raster::extract(landcover, ., buffer = 1100) %>%
                                                    purrr::map_dbl(~ table(.x) %>%
                                                                prop.table() %>%
                                                                .['21'] %>%
                                                                as.numeric() %>%
                                                                ifelse(is.na(.), 0, .))))

    # developed.med_400_sf
    out$developed.med_400 <- unlist(purrr::map(loc_sf$data, ~raster::extract(landcover, ., buffer = 400) %>%
                                                  purrr::map_dbl(~ table(.x) %>%
                                                              prop.table() %>%
                                                              .['23'] %>%
                                                              as.numeric() %>%
                                                              ifelse(is.na(.), 0, .))))

    # developed.low_900_sf
    out$developed.low_900 <- unlist(purrr::map(loc_sf$data, ~raster::extract(landcover, ., buffer = 900) %>%
                                                   purrr::map_dbl(~ table(.x) %>%
                                                              prop.table() %>%
                                                              .['22'] %>%
                                                              as.numeric() %>%
                                                              ifelse(is.na(.), 0, .))))


    # developed.high_1500_sf
    out$developed.high_1500 <- unlist(purrr::map(loc_sf$data, ~raster::extract(landcover, ., buffer = 1500) %>%
                                                     purrr::map_dbl(~ table(.x) %>%
                                                                prop.table() %>%
                                                                .['24'] %>%
                                                                as.numeric() %>%
                                                                ifelse(is.na(.), 0, .))))

    return(out)
}

