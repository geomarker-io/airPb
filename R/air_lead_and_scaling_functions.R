#' @importFrom dplyr %>%
#' @import randomForest
NULL

#' Calculate air lead exposure estimates at specific locations.
#'
#' \code{calculate_airPb()} uses a land use random forest model developed by Dr. Cole Brokamp
#'     based on ambient air sampling in Cincinnati, OH between 2001 and 2005 to estimate
#'     exposure to airborne lead at point locations in the area specified by latitude and longitude.
#'     The model predictors include greenspace (NDVI) within 1000 meters, population density within
#'     500 meters, length of bus routes within 900 meters, percent pasure within 800 meters,
#'     percent developed open land within 1100 meters, percent developed medium land within
#'     400 meters, percent developed low land within 900 meters, and percent developed
#'     high land within 1500 meters.
#'
#' @param locations Data.frame with columns 'id', 'lat', and 'lon' at minimum.
#' @param return.LU.vars When \code{return.LU.vars = TRUE}, the land use predictors used
#'     to generate the air lead values are also returned.
#'
#' @return If \code{return.LU.vars = FALSE}, a numeric vector of air lead estimates (ug/m3) ?
#'     is returned. If \code{return.LU.vars = TRUE}, the \code{locations} data.frame with
#'     additional columns for air lead values and the land use predictors used
#'     to generate the air lead values is returned.
#'
#' @references Cole Brokamp, Roman Jandarov, MB Rao, Grace LeMasters, Patrick Ryan. Exposure assessment
#'     models for elemental components of particulate matter in an urban environment:
#'     A comparison of regression and random forest approaches. Atmospheric Environment. 151. 1-11. 2017.
#'     \url{http://dx.doi.org/10.1016/j.atmosenv.2016.11.066}
#'
#' @examples
#' my_data <- data.frame(id = 1:3,
#'     lat = c(39.19674, 39.12731,	39.28765),
#'     lon = c(-84.58260, -84.52700, -84.51017))
#'
#' lead_est <- calculate_airPb(my_data, return.LU.vars = FALSE)
#' lead_est <- calculate_airPb(my_data, return.LU.vars = TRUE)
#' @export

calculate_airPb <- function(locations, return.LU.vars=FALSE) {

  if(!"id" %in% colnames(locations)) {stop("locations dataframe must have a column called 'id'")}
  if(!"lat" %in% colnames(locations)) {stop("locations dataframe must have a column called 'lat'")}
  if(!"lon" %in% colnames(locations)) {stop("locations dataframe must have a column called 'lon'")}

  missing <- locations %>%
    dplyr::filter(is.na(lat), is.na(lon))

  if (nrow(missing) > 0) {warning(paste0(missing$n,
                                     " observations were missing lat/lon coordinates and will be excluded."))}

  out <- make_spatial_predictors(locations)
  out$airPb <- stats::predict(pb_rf_model, newdata = out)

  if (return.LU.vars == FALSE) {
    out <- out$airPb
  }

  return(out)
}


# d <- data.frame(id = 1:3, lon = c(-83.191771, -84.517997, -84.617997), lat = c(40.0204212, 39.1502776, 39.2502776))
# calculate_air_lead(d, return.LU.vars = FALSE)





#' Calculate temporal scaling factors based on EPA measurements of airborne lead.
#'
#' \code{calculate_scaling_factors()} constructs temporal scaling factors based on measurements
#'     of airborne lead recorded by the EPA in the Cincinnati area. These scaling factors are
#'     the average lead measured over the time period specified by \code{start_date} and
#'     \code{end_date}, divided by the average lead recorded over the ambient air sampling period (2001 to 2005).
#'     Scaling factors can be multiplied by air lead estimates from \code{calculate_airPb()} to adjust for
#'     temporal variability in airborne lead in the Cincinnati area over time.
#'
#' @param dates A data.frame with 2 columns called 'start_date' and 'end_date' at minimum.
#'     Both columns must be of class \code{Date}. See \code{\link{as.Date}} for help converting
#'     a character vector to a Date vector.
#'
#' @return A numeric vector of temporal scaling factors.
#'
#' @details EPA data in this package is available from November 9, 2001
#'     through November 28, 2018. Scaling factors that attempt to average over
#'     air lead measured on dates outside this range will not be calculated. In addition,
#'     it is important to be mindful of the frequency of air lead measurements recorded by the EPA.
#'     Note that air lead was measured every 6 days through the end of 2010, and every 3 days starting in 2011.
#'     If there are less than 4 measurements of air lead between the start_date and end_date, the scaling
#'     factor will not be calculated and NA will be returned.
#'
#' @examples
#' my_dates <- data.frame(start_date = c("2010-01-08", "2012-06-08", "2010-01-09",
#'                                       "2015-04-09", "2010-01-10"),
#'                        end_date = c("2010-02-08", "2012-07-08", "2010-02-09",
#'                                     "2015-05-09", "2010-02-10"))
#'
#' \dontrun{
#' class(my_dates$start_date)  # character vector
#' scaling1m <- calculate_scaling_factors(my_dates)
#' }
#'
#' my_dates$start_date <- as.Date(my_dates$start_date)
#' my_dates$end_date <- as.Date(my_dates$end_date)
#' scaling1m <- calculate_scaling_factors(my_dates)
#' @export

calculate_scaling_factors <- function(dates) {
  if(!"start_date" %in% colnames(dates)) {stop("The dates dataframe must have a column called 'start_date'.")}
  if(!"end_date" %in% colnames(dates)) {stop("The dates dataframe must have a column called 'end_date'.")}

  if (class(dates$start_date) != "Date" | class(dates$end_date) != "Date") {
    stop("Start and end dates are not of class 'Date'. See ?as.Date() to convert.")
  }

  early_dates <- which(dates$start_date < min(lead_ts$date))
  late_dates <- which(dates$end_date > max(lead_ts$date))
  if (length(early_dates) > 0) {
    warning(paste0(length(early_dates), " scaling factor(s) were not computed due to lack of EPA data before ",
                   min(lead_ts$date)))
  }

  if (length(late_dates) > 0) {
    warning(paste0(length(late_dates), " scaling factor(s) were not computed due to lack of EPA data after ",
                   max(lead_ts$date)))
  }

  denom <- lead_ts %>%
    dplyr::filter(date < "2006-01-01") %>%
    dplyr::summarize(mean = mean(Lead, na.rm=TRUE)) %>%
    .$mean

  dates <- dates %>%
    dplyr::mutate(monthly_mean = purrr::map2_dbl(start_date, end_date,
                                                 ~{leads <- dplyr::filter(lead_ts, date >= .x, date <= .y)$Lead
                                                 if (length(leads) < 4) warning("Less than 4 measurements recorded between start and end dates. Returning NA. Consider increasing date range.",
                                                                              call. = FALSE)
                                                 ifelse(length(leads) < 4, NA, mean(leads, na.rm=TRUE))
                                   }),
           scaling_factor = ifelse(start_date < min(lead_ts$date) | end_date > max(lead_ts$date),
                                   NA, monthly_mean/denom)) %>%
    dplyr::select(-monthly_mean)

  return(dates$scaling_factor)
}

#ncalculate_scaling_factors(dates = data.frame(start_date = as.Date("2010-01-01"), end_date = as.Date("2010-02-01")))






#' Calculate temporally scaled air lead exposure estimates at specific locations.
#'
#' \code{calculate_scaled_airPb()} is a wrapper function that estimates airborne lead exposures
#'     at provided locations by calling \code{calculate_airPb()}, then temporally scales those
#'     estimates using scaling factors computed by calling \code{calculate_scaling_factors()}.
#'     This function is particularly useful for calculating exposures at the same locations on
#'     different dates.
#'
#' @param locations Data.frame with columns 'id', 'lat','lon', 'start_date', and 'end_date' at minimum.
#'
#' @return A numeric vector of air lead estimates (ug/m3).
#'
#' @references Cole Brokamp, Roman Jandarov, MB Rao, Grace LeMasters, Patrick Ryan. Exposure assessment
#'     models for elemental components of particulate matter in an urban environment:
#'     A comparison of regression and random forest approaches. Atmospheric Environment. 151. 1-11. 2017.
#'     \url{http://dx.doi.org/10.1016/j.atmosenv.2016.11.066}
#'
#' @examples
#' my_data <- data.frame(id = rep(1,3),
#'     lat = c(39.19674, 39.19674,	39.19674),
#'     lon = c(-84.58260, -84.58260, -84.58260),
#'     start_date = c(as.Date("2010-01-08"), as.Date("2012-06-08"), as.Date("2015-04-09")),
#'     end_date = c(as.Date("2010-02-08"), as.Date("2012-07-08"), as.Date("2015-05-09")))
#'
#' airPb_scaled <- calculate_scaled_airPb(my_data)
#' @export

calculate_scaled_airPb <- function(locations) {

  locations <- d
  unique_locations <- locations %>%
    dplyr::filter(!(duplicated(lat) & duplicated(lon)))

  lead_unadj <- calculate_airPb(unique_locations, return.LU.vars = TRUE)
  lead_unadj <- dplyr::left_join(locations, lead_unadj, by=c("id", "lat", "lon"))

  scaling_factors <- calculate_scaling_factors(dates = locations)

  lead_adj <- lead_unadj$airPb * scaling_factors

  return(lead_adj)
}

# d <- tibble::tribble(
#   ~id,         ~lon,        ~lat,        ~date,
#   809089L, -84.69127387, 39.24710734, as.Date("2010-01-08"),
#   809089L, -84.69127387, 39.24710734, as.Date("2010-02-08"),
#   809089L, -84.69127387, 39.24710734, as.Date("2010-03-08"),
#   799697L, -84.41741798, 39.18541228, as.Date("2010-01-10"),
#   799697L, -84.41741798, 39.18541228, as.Date("2012-02-10")
# )
#
# d <- d %>%
#   rename(start_date = date) %>%
#   group_by(id) %>%
#   mutate(end_date = lead(start_date)) %>%
#   filter(!is.na(end_date)) %>%
#   ungroup()
#
# d %>%
#   mutate(scaled_airPb = calculate_scaled_airPb(.))




