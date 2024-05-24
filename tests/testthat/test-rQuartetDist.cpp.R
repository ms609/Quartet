test_that("invalid `edge` fails gracefully", {
  expect_error(tqdist_QuartetAgreementEdge(matrix(0, 3, 3), matrix(0, 3, 2)),
               "`edge` must .* two columns")
})
