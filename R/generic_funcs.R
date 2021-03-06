aoc_generic_parse_blankrow_groups <- function(path, .summarise = TRUE){
        output_base <- path  %>%
                readr::melt_tsv()  %>%
                dplyr::mutate(
                        double_new = data_type == "missing"
                        , row = cumsum(double_new)
                )  %>%
                dplyr::filter(data_type != "missing")
        if(.summarise){
                output_base %>%
                        dplyr::group_by(row)  %>%
                        dplyr::summarise(all_value = paste0(value, collapse = " "))
        } else {
                output_base
        }
}
