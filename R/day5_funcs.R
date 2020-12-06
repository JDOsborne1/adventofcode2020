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
