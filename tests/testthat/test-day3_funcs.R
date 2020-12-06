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


        angle_list <- list(
                c(1, 1)
                , c(3 , 1)
                , c(5 , 1)
                , c(7 , 1)
                , c(1 , 2)
        )
        list_of_collisions <- purrr::map(angle_list, function(x) check_vs_spec(test_in, x[1], x[2]))  %>%
                as.numeric()
  expect_equal(2 * 2, 4)
})
