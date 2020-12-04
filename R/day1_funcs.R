
#'@export
aoc_day1_expense_report_expansion <- function(input_list, ncomb = 2){
        input_list  %>%
                as.numeric() %>%
                combn(ncomb)  %>%
                t()  %>%
                tibble::as_tibble()  %>%
                tibble::rownames_to_column()  %>%
                tidyr::pivot_longer(cols = tidyr::starts_with("V"))  %>%
                dplyr::group_by(rowname)  %>%
                dplyr::mutate(total = sum(value))  %>%
                dplyr::filter(total == "2020")  %>%
                dplyr::mutate(product = prod(value))  %>%
                dplyr::pull(product)  %>%
                unique()
}

#* Return the product of the two entries that sum to 2020
#* @param input_list the list of entries
#* @post /day1_product_part1
aoc_day1_expense_report_expansion_wrapper <- function(input_list){
        reformed_input <- jsonlite::fromJSON(input_list)
        aoc_day1_expense_report_expansion(reformed_input, 2)
}
#* Return the product of the three entries that sum to 2020
#* @param input_list the list of entries
#* @post /day1_product_part2
aoc_day1_expense_report_expansion_wrapper_part2 <- function(input_list){
        reformed_input <- jsonlite::fromJSON(input_list)
        aoc_day1_expense_report_expansion(reformed_input, 3)
}
#* Return the product of the three entries that sum to 2020
#* @param input_list the list of entries
#* @post /day1_product_flex
aoc_day1_expense_report_expansion_wrapper_flex <- function(input_list, ncomb){
        reformed_input <- jsonlite::fromJSON(input_list)
        reformed_ncomb <- as.integer(ncomb)
        aoc_day1_expense_report_expansion(input_list = reformed_input, ncomb = reformed_ncomb)
}
