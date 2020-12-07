aoc_day7_importer_function <- function(path){
        path  %>%
                readr::read_lines() %>%
                enframe(name = NULL, value = "rules_raw")
}

aoc_day7_parser_function <- function(raw_rules_tbl){
        raw_rules_tbl  %>%
                separate(rules_raw, into = c("container", "contents"), sep = "contain")  %>%
                mutate_at(vars(contents), str_replace_all, "\\.", "")  %>%
                mutate_at(vars(contents), str_split, ",")  %>%
                unnest_longer(contents)  %>%
                mutate(num = str_extract(contents, "\\d"))  %>%
                mutate_at(vars(contents), str_replace_all, "\\d+\\s", "")  %>%
                mutate_at(vars(contents), str_replace_all, "s$", "")  %>%
                mutate_at(vars(contents), str_replace_all, "^\\s", "")   %>%
                mutate_at(vars(contents), str_replace_all, "\\s$", "")   %>%
                mutate_at(vars(container), str_replace_all, "\\s$", "")   %>%
                mutate_at(vars(container), str_replace_all, "^\\s", "")   %>%
                mutate_at(vars(container), str_replace_all, "s$", "")
}



### Section of shame ---------------


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
