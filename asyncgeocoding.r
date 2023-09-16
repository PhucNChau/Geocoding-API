library("crul")
library("curl")
#library(rvest)
#library(stringr)
library(jsonlite)
base_census_url <-
  "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"

q <- "benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json&layers=6&address=6500 Tabor Ave, Philadelphia, PA 19111"

temp <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress?benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json&layers=6&address=6500 Tabor Ave, Philadelphia, PA 19111"
temp2 <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress?benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&format=json&layers=6&address=2113 N 17th st, Philadelphia, PA 19121"

bodies <- list()

done_function <- function(x) {
  if (x$status == 200) {
    data <- fromJSON(rawToChar(x$content))
    if (length(data$result) > 0 && length(data$result$addressMatches) > 0) {
      geoid <- data$result$addressMatches$geographies$`Census Tracts`[[1]]$GEOID
      bodies <<- append(bodies, geoid)
    } else {
      bodies <<- append(bodies, "GEOID not found")
    }
  } else {
    bodies <<- append(bodies, "An error occur")
  }
}

pool <- new_pool()
curl_fetch_multi(URLencode(temp), done = done_function, pool = pool)
curl_fetch_multi(URLencode(temp2), done = done_function, pool = pool)

out <- multi_run(pool = pool)
print(out)