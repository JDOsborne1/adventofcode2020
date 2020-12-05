aoc_day4_test_passport_data <- function(input_data, passport_values){
        col_existance <- all(passport_values %in% input_data$X1)

        byr_test <- input_data  %>%
                filter()

        #byr (Birth Year) - four digits; at least 1920 and at most 2002.
        #iyr (Issue Year) - four digits; at least 2010 and at most 2020.
        #eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
        #hgt (Height) - a number followed by either cm or in:
        #If cm, the number must be at least 150 and at most 193.
        #If in, the number must be at least 59 and at most 76.
        #hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
        #ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
        #pid (Passport ID) - a nine-digit number, including leading zeroes.
        #cid (Country ID) - ignored, missing or not.

}
aoc_day4_test_length <- function(string, req_length){
        stringr::str_length(string) == req_length
}
aoc_day4_test_range <- function(string, min_val, max_val, converter){
        converter(string) >= converter(min_val) && converter(string) <= converter(max_val)
}
aoc_day4_test_prefix <- function(string, req_pref){
        any(stringr::str_detect(string, paste0("^", req_pref)))
}
aoc_day4_test_suffix <- function(string, req_suff) {
        any(stringr::str_detect(string, paste0(req_suff, "$")))
}
aoc_day4_test_membership <- function(string, mem_list) {
        string %in% mem_list
}

x = "2030"
aoc_day4_apply_tests <- function(input_data_wide ){
        input_data_wide  %>%
        mutate_at(
                vars(iyr)
                , map
                , function(x) {
                        aoc_day4_test_length(x, 4) &&
                        aoc_day4_test_range(x, 2010, 2020, as.numeric) &&
                        !is.na(x)
                }
        )  %>%
        mutate_at(
                vars(byr)
                , map
                , function(x) {
                        aoc_day4_test_length(x, 4) &&
                        aoc_day4_test_range(x, 1920, 2002, as.numeric) &&
                        !is.na(x)
                }
        )  %>%
        mutate_at(
                vars(eyr)
                , map
                , function(x) {
                        aoc_day4_test_length(x, 4) &&
                        aoc_day4_test_range(x, 2020, 2030, as.numeric) &&
                        !is.na(x)
                }
        )  %>%
        mutate_at(
                vars(hgt)
                , map
                , function(x) {
                        aoc_day4_test_prefix(x, "\\d") &&
                        (
                                (
                                        aoc_day4_test_suffix(x, "in")
                                        && aoc_day4_test_range(x, 59, 76, function(x) as.numeric(gsub("\\D", "", x)))
                                )
                                |
                                (
                                        aoc_day4_test_suffix(x, "cm")
                                        && aoc_day4_test_range(x, 150, 193, function(x) as.numeric(gsub("\\D", "", x)))
                                )
                        ) &&
                        !is.na(x)
                }
        )  %>%
        mutate_at(
                vars(hcl)
                , map
                , function(x) {
                        aoc_day4_test_length(x, 7) &&
                        aoc_day4_test_prefix(x, "#") &&
                        aoc_day4_test_suffix(x, "[a-f0-9]{6}") &&
                        !is.na(x)
                }
        )  %>%
        mutate_at(
                vars(ecl)
                , map
                , function(x) {
                        aoc_day4_test_membership(x, c(
                                "amb"
                                ,"blu"
                                ,"brn"
                                ,"gry"
                                ,"grn"
                                ,"hzl"
                                , "oth"
                        )) &&
                        !is.na(x)
                }
        )  %>%
        mutate_at(
                vars(pid)
                , map
                , function(x) {
                        aoc_day4_test_length(x, 9) &&
                        aoc_day4_test_suffix(x, "[0-9]{9}") &&
                        !is.na(x)
                }
        )
}
