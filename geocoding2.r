library(httr)
library(sf)

get_geoid <- function(address) {
    base_census_url <-
        "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"
    # Call Census API
    query_params <- list(
        address = address,
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
            !is.null(data$result$addressMatches)) {
        geoid <- data$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$GEOID
        return(geoid)
        } else {
        print("No geoid found.")
        }
    } else {
        print("Invalid response from Census API.")
    }
}

# Method 2: Get geoid from Census using address
addresses <- c("2114 N Gratz St, Philadelphia, PA 19121",
               "1605 W Allegheny Avenue, Philadelphia, Pennsylvania, 19132")


for (address in addresses) {
  geoid <- get_geoid(address)
  print(geoid)
}
