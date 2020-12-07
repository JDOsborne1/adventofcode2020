library(dplyr)
library(tidyr)
library(tibble)
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

# url for local testing
url <- "http://127.0.0.1:8000"

# read example stock data
.data <- datasets::mtcars

day_1_formatted <- system.file("intdata", "day1_input.txt", package = "adventofcode2020")  %>%
readr::read_csv(col_names = c("test"))  %>%
pull(test)
library(adventofcode2020)




# create example body
body <- list(
  #msg = "Testing Testing 123"
  #, spec = "versicolor"
  #, indat = toJSON(head(.data))
   input_pwd_audit = toJSON(day_2_formatted)
)
# set API path
path <- 'day2_legal_passwords'

# send POST Request to API
#raw.result <- GET(url = url, path = path,  encode = 'json')

# send POST Request to API
raw.result <- httr::POST(url = url, path = path, query = body, encode = 'json')

# check status code
raw.result$status_code
# retrieve transformed example stock data
.t_data <- fromJSON(rawToChar(raw.result$content))
print(.t_data)
