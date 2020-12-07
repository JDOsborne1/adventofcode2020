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


day_7_formatted <- aoc_day7_importer_function(

)
day_7_parsed <- aoc_day7_parser_function(
        raw_rules_tbl = day_7_formatted
)

day_7_parsed %>% filter(contents == "dim red bag")

initial_bag <- tibble(contents = "bright gold bag")
initial_bag  %>%
        left_join(day_7_parsed, by = "contents")  %>%
        rename( contents1 = contents, num1 = num, contents = container)  %>%
        left_join(day_7_parsed, by = "contents")   %>%
        head(1)  %>%
        pull(contents)

        library(tidyverse)

data <- read_lines("/Users/user/Documents/adventofcode2020/inst/intdata/day7_input.txt") %>%
          str_split(" contain ") %>%
          map(~ str_split(.x, ", ") %>% unlist()) %>%
          set_names(., map_chr(., ~ magrittr::extract2(., 1)) %>% str_remove(" bags?")) %>%
          map(~ magrittr::extract(., -1))


        ## part1
        find_colors <- function(list, color) {
          tmp <- c()
          for (i in color) {
            tmp <- c(tmp, map_dbl(list, ~ str_detect(., i) %>% sum()) %>%
                       magrittr::extract(.>0) %>%
                       names() %>%
                       str_remove(" bags?"))
          }
          return(tmp)
        }

        final <- c()
        k <- "shiny gold"

        while(length(k) != 0) {
          k <- find_colors(data, k)
          final <- c(final, k)        # don't do this in real code
        }

        ## (slow) Answer:
        length(unique(final))


df <- data %>%
          map( ~ list(amounts = .x %>% str_extract(., "[0-9]+"), bags = .x %>% str_extract(., "[a-zA-Z]+ [a-zA-Z]+"))) %>%
          bind_rows(.id = "bag") %>%
          mutate(amounts = as.numeric(amounts))

        part2 <- function(df, bag) {

          multiplier <- table(bag) %>%
            as_tibble()

          tmp <- df %>%
            filter(bag %in% !!bag) %>%
            select(bag, bags, amounts) %>%
            filter(bags != "no other", !is.na(amounts)) %>%
            left_join(multiplier, by = c(bag = "bag")) %>%
            mutate(amounts = amounts * n)

          map2(tmp$bags, tmp$amounts, ~ rep(.x, .y)) %>%
            unlist()
        }

        k <- part2(df, "shiny gold")
        bags <- length(k)

        while(!is.null(k)) {
          k <- part2(df, k)
          bags <- bags + length(k)
        }

        ## Answer
        bags

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
