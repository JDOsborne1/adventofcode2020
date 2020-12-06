aoc_day6_get_unique_letters <- function(parsed_input){
        parsed_input  %>%
                mutate(string_list = map(all_value, strsplit, "")) %>%
                #mutate(string_list_unique = map(string_list, function(x) map(x, unique)))  %>%
                select(row, string_list)  %>%
                unnest_longer(col = string_list)  %>%
                unnest_longer(col = string_list)  %>%
                filter(string_list != " ")  %>%
                distinct(row, string_list)
}

aoc_day6_get_part1 <- function(input_list){
        input_list  %>%
                count(row)  %>%
                summarise(total = sum(n))
}
aoc_day6_get_part2 <- function(input_list){
        input_list  %>%
                group_by(row)  %>%
                mutate(
                        n_people = n_distinct(rownum)
                )  %>%
                ungroup()  %>%
                group_by(row, value)  %>%
                summarise(
                        count_letter = n()
                        , prop_letter = count_letter/unique(n_people)
                )  %>%
                ungroup()  %>%
                filter(prop_letter == 1)  %>%
                count(row)  %>%
                summarise(total_count = sum(n))
}


aoc_day6_get_common_letters <- function(parsed_input){
        test_table <- parsed_input  %>%
                mutate(string_list = map(value, strsplit, "")) %>%
                #unnest_longer(col = string_list)  %>%
                select(row, string_list)  %>%
                group_by(row)  %>%
                mutate(rownum = 1:n())  %>%
                ungroup()  %>%
                mutate(string_df = map(string_list, function(x) map(x, aoc_day6_list_into_df))) %>%
                select(row, rownum, string_df)  %>%
                unnest(string_df)  %>%
                unnest(string_df)
}

aoc_day6_list_into_df <- function(input_list){
        output <- input_list  %>%
                {`names<-`(., .)}
        output %>%
                as_tibble_col()  %>%
                mutate(value = as.character(value))
}
