library(httr)
library(sf)

get_coordinate <- function(address) {
  api_key <- ""
  base_here_url <- "https://geocode.search.hereapi.com/v1/geocode"

  query_params <- list(
    q = address,
    apiKey = api_key
  )
  response <- GET(base_here_url, query = query_params)

  # Parse the response to extract coordinates
  if (http_type(response) == "application/json") {
    data <- content(response, "parsed")
    if (!is.null(data$items) && length(data$items) > 0) {
      coordinates <- data$items[[1]]$position
      latitude <- coordinates$lat
      longitude <- coordinates$lng
      coordinate <- list("lat" = latitude, "long" = longitude)
      return(coordinate)
    } else {
      print("No coordinate found.")
    }
  } else {
    print("Invalid response from Here API.")
  }
}

get_geoid <- function(coordinate) {
  base_census_url <-
    "https://geocoding.geo.census.gov/geocoder/geographies/coordinates"
  if (length(coordinate) == 2) {
    # Call Census API
    query_params <- list(
      x = coordinate$long,
      y = coordinate$lat,
      benchmark = "Public_AR_Census2020",
      vintage = "Census2020_Census2020",
      format = "json",
      layers = 6
    )

    response <- GET(base_census_url, query = query_params)

    # Parse the response to extract geoid
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      if (!is.null(data$result) && length(data$result) > 0 &&
            !is.null(data$result$geographies)) {
        geoid <- data$result$geographies$`Census Tracts`[[1]]$GEOID
        return(geoid)
      } else {
        print("No geoid found.")
      }
    } else {
      print("Invalid response from Census API.")
    }
  }
}

# Method 1: Get lat, long from Here then call Census to get geoid
addresses <- c("2114 N Gratz St, Philadelphia, PA 19121",
               "1605 W Allegheny Avenue, Philadelphia, Pennsylvania, 19132")


for (address in addresses) {
  coordinate <- get_coordinate(address)
  geoid <- get_geoid(coordinate)
}
