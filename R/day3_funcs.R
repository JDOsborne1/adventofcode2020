
aoc_day3_parse_input <- function(input_data){
        map_width <- input_data  %>% head(1)  %>% pull(X1)  %>% stringr::str_length()
        input_data  %>%
                tidyr::separate(X1, into = as.character(0:map_width), sep = "")  %>%
                select(-`0`)  %>%
                as.matrix()
}
#parsed_test <- test_in  %>% aoc_day3_parse_input()

#map_length <- nrow(parsed_test)
#map_width <- ncol(parsed_test)

gradient <- c(1,3)

pr_chr <- function(char, num){
        paste0(rep(char, num), collapse = "")
}

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

aoc_day3_generate_selector_matrix <- function(planned_width, planned_length, delta_x, delta_y){
        standard_tick_length <- (planned_width * delta_y) + (delta_x - 1)
        wrapping_tick_length <- (planned_width * (delta_y-1)) + (delta_x - 1)

        std_append <- c(rep(".", standard_tick_length), "#") #Standard tick
        wrapping_append <-  c(rep(".", wrapping_tick_length), "#") # Wrapping tick

        vect <- c("#")
        pos <- 1
        for(i in 1:(ceiling(planned_length/delta_y))){
                if(pos + delta_x <= planned_width){
                        vect <- c(vect, std_append)
                        pos <- length(vect) %% planned_width
                        if(pos==0) pos <- planned_width
                } else {
                        vect <- c(vect,wrapping_append)
                        pos <- length(vect) %% planned_width
                        if(pos==0) pos <- planned_width
                }
        }

        total_elements <- planned_length * planned_width

        matrix(vect[1:total_elements], ncol = planned_width, byrow = T)
}


jump_vector <- function(input_vect, null_input, jump_vect, width){
        new_vect <- input_vect + jump_vect
        new_vect[1] <- new_vect[1] %% width
        if(new_vect[1]==0) new_vect[1] <- width
        return(new_vect)

}
aoc_day3_generate_selector_matrix_unused <- function(planned_width, planned_length, delta_x, delta_y){
        num_jumps <- ceiling(planned_length/delta_y)
        empty_matrix <- rep(".", planned_width * planned_length)  %>% matrix(ncol = planned_width, byrow = TRUE)
        jump_list <- accumulate(1:(num_jumps-1), jump_vector, jump_vect = c(1, 2), width = planned_width, .init = c(1, 1))
        jump_list[[1]] <- c(1,1)
        for(i in jump_list){
                empty_matrix[i[2], i[1]] <- "#"
        }
        #empty_matrix[jump_selector]
        return(empty_matrix )

}

test_mat <- aoc_day3_generate_selector_matrix(
        planned_width = 11
        , planned_length = 11
        , delta_x = 3
        , delta_y = 1
)
#test_mat_int <- 1*(test_mat == "#")
#parsed_test_int <- 1*(parsed_test == "#")

#sum(test_mat_int * parsed_test_int)
