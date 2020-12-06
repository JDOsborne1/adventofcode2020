testing_file <- system.file("intdata", "day5_input.txt", package = "adventofcode2020")  %>%
        readr::read_table(col_names = FALSE)  %>%
        aoc_day5_parse_input()
test_that("pass interpretation works", {
        test_string <- "FBFBBFFRLR"
        test_output <- aoc_day5_parse_pass_code(test_string, vert_cols = 0:127, horiz_rows = 0:7)
        expect_equal(test_output, list(row = 44, col = 5))

})
test_that("dataframe level pass interpretation works", {
        test_input <- data.frame(pass_code = c("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"))
        test_output <- test_input   %>%
                mutate(position_out = map(as.character(pass_code), aoc_day5_parse_pass_code, vert_cols = 0:127, horiz_rows = 0:7))  %>%
                unnest_wider(position_out)  %>%
                mutate(seat_id = (row*8) + col)
        expect_equal(test_output, list(col = 44, row = 5))

})


test_that("vector splitting works", {
        expect_equal(aoc_day5_split_vector(0:127, "F"), 0:63)
        expect_equal(aoc_day5_split_vector(0:127, "B"), 64:127)
        expect_equal(aoc_day5_split_vector(0:7, "L"), 0:3)
        expect_equal(aoc_day5_split_vector(0:7, "R"), 4:7)

})
