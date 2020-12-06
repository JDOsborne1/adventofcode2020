test_that("multiplication works", {
        test_string <- "gwlcpbtxmiezd xwlcpbtzimgdk tbiwmpcgzdxo ygzdbpjxncfwimt"
        test_list <- strsplit(test_string, split = "")
        purrr::map(test_list, unique)
  expect_equal(2 * 2, 4)
})
