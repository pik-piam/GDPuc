test_that("multiplication works", {
  expect_message(print_source_info("wb_wdi"))
  expect_message(
    expect_message(
      expect_message(
         print_source_info()
      )
    )
  )
})
