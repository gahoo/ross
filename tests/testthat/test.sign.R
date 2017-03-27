test_that("sign", {
  expect_equal(.build.signature('GET', '/', expires = 0), "RPOyKTMuG0+f+TKzVWW2oo7+zAY=")

  r<-.sign.header('GET', "http://oss.aliyuncs.com", "/")
  expect_false(http_error(r))

  r<-.sign.url('GET', "http://igenecode.oss-cn-beijing.aliyuncs.com/ross%2Fross.txt", "/igenecode/ross/ross.txt")
  expect_false(http_error(r))

})
