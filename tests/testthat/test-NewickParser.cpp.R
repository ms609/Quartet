test_that("NewickParser handles empty input", {
  expect_error(tqdist_OneToManyQuartetAgreementChar(character(0), character(0)),
               "string_in has length 0")
  expect_error(tqdist_QuartetAgreementChar(character(0), "(t1, (t2, t3));"),
               "string_in has length 0")
  expect_error(tqdist_QuartetAgreementChar("(t1, (t2, t3));", character(0)),
               "string_in has length 0")
})
