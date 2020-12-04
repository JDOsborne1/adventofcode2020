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

day_3_formatted <- readr::read_table(
        "/Users/user/Documents/adventofcode2020/inst/intdata/day3_input.txt"
        , col_names = FALSE
)
parsed_test <- day_3_formatted  %>% aoc_day3_parse_input()

check_vs_spec <- function(hash_map, delta_x, delta_y){

        test_mat <- aoc_day3_generate_selector_matrix(
                planned_width = dim(hash_map)[2]
                , planned_length = dim(hash_map)[1]
                , delta_x = delta_x
                , delta_y = delta_y
        )
        test_mat_int <- 1*(test_mat == "#")
        parsed_test_int <- 1*(parsed_test == "#")

        sum(test_mat_int * parsed_test_int)

}

list(
        c()
)


aoc_day2_count_legal_passwords_v2(day_2_formatted)

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
