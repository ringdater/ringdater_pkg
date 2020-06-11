context("comb.na")
library(ringdater)

test_that("comb.NA combines two data.frames of differing nrows", {
  df1<-data.frame(a=1:10, b=2:11)
  df2<-data.frame(c=5:10, d=6:11)
  expect_equal(ncol(comb.NA(df1,df2)), 4)
  expect_equal(nrow(comb.NA(df1,df2)), 10)
})

