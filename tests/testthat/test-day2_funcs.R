test_that("processing works", {
        tempfil <- tempfile()
        writeLines("1-3 a: abcde\n
1-3 b: cdefg\n
2-9 c: ccccccccc" , con = tempfil)
        test_in <- readr::read_delim(
                tempfil
                , delim = ' '
                , col_names = FALSE
        )
  expect_equal(aoc_day2_count_legal_passwords(test_in), 2)
  expect_equal(aoc_day2_count_legal_passwords_v2(test_in), 1)
})


test_that("String Counting works", {
        ex_string <- "gghghgg"
        expect_equal(stringr::str_count(ex_string, "g"), 5)
        expect_equal(stringr::str_count(ex_string, "h"), 2)

})


test_that("Range Checking works", {
        test_max <- 10
        test_min <- 2

        expect_false(aoc_day2_in_range_check(1, test_min, test_max))
        expect_true(aoc_day2_in_range_check(5, test_min, test_max))
        expect_false(aoc_day2_in_range_check(11, test_min, test_max))

})
