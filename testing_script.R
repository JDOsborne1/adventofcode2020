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



day_5_formatted <- readr::read_table(
        "/Users/user/Documents/adventofcode2020/inst/intdata/day5_input.txt"

        , col_names = FALSE
)  %>% aoc_day5_parse_input()

day5_output <- day_5_formatted  %>%
        mutate(position_out = map(as.character(pass_code), aoc_day5_parse_pass_code, vert_cols = 0:127, horiz_rows = 0:7))  %>%
        unnest_wider(position_out)  %>%
        mutate(seat_id = (row*8) + col)


day5_output  %>% summarise(max_seat = max(seat_id))
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
