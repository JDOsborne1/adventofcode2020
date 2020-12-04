test_that("Expansion and Product Works", {
        test_list <- c(
                "1721"
                ,"979"
                ,"366"
                ,"299"
                ,"675"
                ,"1456"
        )

  expect_equal(
          aoc_day1_expense_report_expansion(test_list), 514579)
})
