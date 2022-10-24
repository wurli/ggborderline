testthat::test_that("borderlines work", {

  df <- data.frame(
    x = rep(1:5, 2),
    y = c(3, 3, 3, 3, 3, 1:5),
    colour = rep(c("a", "b"), each = 5)
  )

  ggplot(df, aes(x, y, colour = colour)) +
    geom_borderline(linewidth = 1, borderwidth = 3)

  ggplot(txhousing, aes(date, median, group = city)) +
    geom_borderline()

})
