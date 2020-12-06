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


day_6_formatted <- aoc_generic_parse_blankrow_groups(
        "/Users/user/Documents/adventofcode2020/inst/intdata/day6_input.txt"

)

day_6_output <- aoc_day6_get_unique_letters(
        parsed_input = day_6_formatted
)

day_6_result_part_1 <- aoc_day6_get_part1(
        input_list = day_6_output
)
day_6_formatted_2 <- aoc_generic_parse_blankrow_groups(
        "/Users/user/Documents/adventofcode2020/inst/intdata/day6_input.txt"
        , .summarise = FALSE
)

day_6_output_part2 <- aoc_day6_get_common_letters(
        parsed_input = day_6_formatted_2
)
day_6_result_part_2 <- aoc_day6_get_part2(
        input_list = day_6_output_part2
)


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
