aoc_day5_parse_input <- function(input_ds){
        input_ds  %>%
                rename(pass_code = X1)
}

aoc_day5_split_vector <- function(input_vector, switch_arg){
        median_loc <- median(input_vector)
        if(switch_arg %in% c("B", "R")){
                input_vector[input_vector > median_loc]
        } else {
                input_vector[input_vector < median_loc]
        }
}
