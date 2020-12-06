aoc_day5_parse_input <- function(input_ds){
        input_ds  %>%
                rename(pass_code = X1)
}


aoc_day5_parse_pass_code <- function(pass_code, vert_cols, horiz_rows){
        split_code <- strsplit(pass_code, split = "")[[1]]
        vert_args <- split_code[split_code %in% c("F", "B")]
        horiz_args <- split_code[split_code %in% c("L", "R")]

        list(
                row = as.numeric(reduce(vert_args, .f = aoc_day5_split_vector, .init = vert_cols))
                ,
                col = as.numeric(reduce(horiz_args, .f = aoc_day5_split_vector, .init = horiz_rows))
        )

        #return(vert_args)

}

aoc_day5_split_vector <- function(input_vector, switch_arg){
        median_loc <- median(input_vector)
        if(switch_arg %in% c("B", "R")){
                input_vector[input_vector > median_loc]
        } else {
                input_vector[input_vector < median_loc]
        }
}


aoc_day5_perform_test <- function(){

        day5_output <- day_5_formatted  %>%
        mutate(position_out = map(as.character(pass_code), aoc_day5_parse_pass_code, vert_cols = 0:127, horiz_rows = 0:7))  %>%
        unnest_wider(position_out)  %>%
        mutate(seat_id = (row*8) + col)


        day5_output  %>% summarise(max_seat = max(seat_id), min_seat = min(seat_id))

        day5_output  %>%
        arrange(seat_id)  %>%
        mutate(index = 70:825)  %>%
        mutate(diff_flag = index != seat_id)
}
