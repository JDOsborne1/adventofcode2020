test_that("multiplication works", {
        tempfil <- tempfile()
        writeLines("..##.......\n
#...#...#..\n
.#....#..#.\n
..#.#...#.#\n
.#...##..#.\n
..#.##.....\n
.#.#.#....#\n
.#........#\n
#.##...#...\n
#...##....#\n
.#..#...#.#\n" , con = tempfil)
        test_in <- readr::read_table(
                tempfil
                , col_names = FALSE
        )
  expect_equal(2 * 2, 4)
})
