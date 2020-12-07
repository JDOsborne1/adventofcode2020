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
