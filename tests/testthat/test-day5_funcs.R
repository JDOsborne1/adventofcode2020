
test_that("vector splitting works", {
        expect_equal(aoc_day5_split_vector(0:127, "F"), 0:63)
        expect_equal(aoc_day5_split_vector(0:127, "B"), 64:127)
        expect_equal(aoc_day5_split_vector(0:7, "L"), 0:3)
        expect_equal(aoc_day5_split_vector(0:7, "R"), 4:7)

})
