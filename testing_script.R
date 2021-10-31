library(dplyr)
library(tidyr)
library(tibble)
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

# url for local testing
url <- "http://127.0.0.1:8000"


library(adventofcode2020)

day_8_input <- readr::read_delim(
        "/Users/user/Documents/adventofcode2020/inst/intdata/day8_input.txt"
        , col_names = FALSE
        , delim = " "
)
day_8_input[1,]
index2 <- 1
global_store2 <- 0
history2 <- list()
for (i in 1:nrow(day_8_input)){
        print(c(day_8_input[index2,]$X1, day_8_input[index2,]$X2))
        aoc_day8_dispatch_func(
                fun = day_8_input[index2,]$X1
                , arg = day_8_input[index2,]$X2
                , index = "index2"
                , global_store = "global_store2"
                , history = "history2"
        )
}

global_store2

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
