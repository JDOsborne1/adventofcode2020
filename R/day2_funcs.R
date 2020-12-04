aoc_day2_count_legal_passwords <- function(input_data){
        output <- input_data  %>%
                {`colnames<-`(., c("Range", "Key", "Password"))}  %>%
                dplyr::mutate_at(dplyr::vars(Key), stringr::str_replace, ":", "")  %>%
                tidyr::separate(Range, into = c("Lower", "Upper"), sep = "-", remove = FALSE)  %>%
                dplyr::mutate(count_key = stringr::str_count(Password, Key))  %>%
                dplyr::mutate_at(dplyr::vars(Lower, Upper), as.numeric)  %>%
                dplyr::mutate(legal = purrr::pmap_lgl(.l = list(count_key, Lower, Upper ), .f = aoc_day2_in_range_check))  %>%
                dplyr::pull(legal)  %>%
                sum()
        return(output)
}
aoc_day2_count_legal_passwords_v2 <- function(input_data){
        output <- input_data  %>%
                {`colnames<-`(., c("Range", "Key", "Password"))}  %>%
                dplyr::mutate_at(dplyr::vars(Key), stringr::str_replace, ":", "")  %>%
                tidyr::separate(Range, into = c("Lower", "Upper"), sep = "-", remove = FALSE)  %>%
                dplyr::mutate(locate_keys = purrr::map2(.x=Password, .y=Key, .f=aoc_day2_extract_list_of_locations))  %>%
                dplyr::mutate_at(dplyr::vars(Lower, Upper), as.numeric)  %>%
                dplyr::mutate(legal = purrr::pmap_lgl(.l = list(locate_keys, Lower, Upper ), .f = aoc_day2_exactly_1_match_check)) %>%
                dplyr::pull(legal)  %>%
                sum()
        return(output)
}

aoc_day2_extract_list_of_locations <- function(pwd, key){
        list_matchlist <- stringr::str_locate_all(pwd, key)
        matrix_matchlist <- list_matchlist[[1]]
        vector_matchlist <- matrix_matchlist[,"start"]
        return(vector_matchlist)
}

aoc_day2_in_range_check <- function(value, lowerbound, upperbound){
        valid_upper <- value <= upperbound
        valid_lower <- value >= lowerbound
        valid_all <- valid_upper && valid_lower
        return(valid_all)
}
aoc_day2_exactly_1_match_check <- function(value, lowerbound, upperbound){
        valid_upper <- upperbound %in% value
        valid_lower <- lowerbound %in% value
        valid_either <- valid_upper || valid_lower
        valid_both <- valid_upper && valid_lower
        valid_criteria <- valid_either && !valid_both
        return(valid_criteria)
}


#* Return the product of the three entries that sum to 2020
#* @param input_list the list of entries
#* @post /day2_legal_passwords
aoc_day2_count_legal_passwords_wrapper <- function(input_pwd_audit){
        reformed_input <- jsonlite::fromJSON(input_pwd_audit)
        output <- aoc_day2_count_legal_passwords(reformed_input)
        return(output)
}
