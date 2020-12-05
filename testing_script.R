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

angle_list <- list(
        c(1, 1)
        , c(3 , 1)
        , c(5 , 1)
        , c(7 , 1)
        , c(1 , 2)
)
list_of_collisions <- map(angle_list, function(x) check_vs_spec(parsed_test, x[1], x[2]))  %>%
        as.numeric()

test_result <- prod(list_of_collisions)
test_result
aoc_day2_count_legal_passwords_v2(day_2_formatted)

day_3_formatted <- readr::melt_tsv(
        #"/Users/user/Documents/adventofcode2020/inst/intdata/day4_input.txt"
        tempfil
        #, col_names = FALSE
)

parsed_lines <- day_3_formatted  %>%
        mutate(
                double_new = data_type == "missing"
                , row = cumsum(double_new)
        )  %>%
        filter(data_type != "missing")  %>%
        group_by(row)  %>%
        summarise(all_value = paste0(value, collapse = " "))

test_list <- parsed_lines  %>%
        mutate(all_value = map_chr(all_value, .f = str_replace_all, " ", "\n")) %>%
        mutate(all_value = map(all_value, .f = readr::read_delim, delim = ":", col_names = FALSE))

passport_values <- c(
        "byr"
        , "iyr"
        , "eyr"
        , "hgt"
        , "hcl"
        , "ecl"
        , "pid"
        #, "cid"
)



test_list  %>%
        mutate(is_valid = map(all_value, aoc_day4_test_passport_data)  %>%
        count(is_valid)

wide_list <- test_list  %>%
        tidyr::unnest(cols = all_value)  %>%
        tidyr::pivot_wider(id_cols = row, names_from = X1, values_from = X2, values_fill = NA)



verified_list <- wide_list  %>%
        select(-cid)  %>%
        #head()  %>%
        aoc_day4_apply_tests()  %>%
        pivot_longer(cols = -row, values_to = "pass_status", names_to = "criteria")  %>%
        mutate_at(vars(pass_status), as.logical)  %>%
        group_by(row)  %>%
        summarise(pass_test = all(pass_status))

verified_list  %>% count(pass_test)

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
