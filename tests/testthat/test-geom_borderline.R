test_that("geom_borderline works", {
  
  dat <- data.frame(
    group = rep(letters[1:3], each = 4),
    x = rep(1:4, 3),
    y = c(sapply(1:3, function(x) 1:4 * x - 5 * log(x))) ^ 2
  ) 
  
  p1 <- ggplot(dat, aes(x, y, colour = group)) +
    geom_borderline(linewidth = 1)
  
  l1 <- layer_data(p1)
  
  # Expect 3 colours
  expect_equal(length(unique(l1$colour)), 3)
  
  # Expect bordercolours all white
  expect_equal(unique(l1$bordercolour), "white")
  
  # Expect linewidth all 1
  expect_equal(unique(l1$linewidth), 1)
  
  
  p2 <- ggplot(dat, aes(x, y, group = group)) +
    geom_borderline(linewidth = 1)
  
  l2 <- layer_data(p2)
  
  # Expect 3 lines
  expect_equal(length(unique(l2$group)), 3)
  
  # Expect all to be black
  expect_equal(unique(l2$colour), "black")
  
})


test_that("size/linewidth work in ggplot2 >= 3.4.0", {
  
  if (compareVersion(as.character(packageVersion("ggplot2")), "3.3.6.9000") < 0) {
    skip("Install ggplot2 3.4.0 or later to run this test")
  }
  
  dat <- data.frame(
    group = rep(letters[1:3], each = 4),
    size = rep(1:3 / 2, each = 4),
    x = rep(1:4, 3),
    y = c(sapply(1:3, function(x) 1:4 * x - 5 * log(x))) ^ 2
  ) 
  
  p1 <- expr(
    ggplot(dat, aes(x, y, colour = group)) +
      geom_borderline(size = 1) 
  )
  
  # Check that the right message is thrown about size being deprecated
  eval(bquote(expect_snapshot(
    invisible(ggplot_gtable(ggplot_build(
      .(plot_expr)
    )))
  )))
    
  l1 <- layer_data(eval(p1))
  
  # Size should be renamed to linewidth
  expect_false("size" %in% colnames(l1))
  
  # Check renaming has been done properly
  expect_equal(unique(l1$linewidth), 1)
})


test_that("size/linewidth work in ggplot2 < 3.4.0", {
  
  if (compareVersion(as.character(packageVersion("ggplot2")), "3.3.6.9000") >= 0) {
    skip("Install ggplot2 3.3.6 or earlier to run this test")
  }
  
  dat <- data.frame(
    group = rep(letters[1:3], each = 4),
    size = rep(1:3 / 2, each = 4),
    x = rep(1:4, 3),
    y = c(sapply(1:3, function(x) 1:4 * x - 5 * log(x))) ^ 2
  ) 
  
  p1 <- ggplot(dat, aes(x, y, colour = group)) +
    geom_borderline(size = 3)
  
  l1 <- layer_data(p1)
  
  expect_true("size" %in% colnames(l1))
  expect_true("linewidth" %in% colnames(l1))
  expect_equal(unique(l1$size), 3)
  
  p2 <- ggplot(dat, aes(x, y, colour = group)) +
    geom_borderline(linewidth = 3)
  
  l2 <- layer_data(p2)
  
  expect_false("size" %in% colnames(l2))
  expect_true("linewidth" %in% colnames(l2))
  expect_equal(unique(l2$linewidth), 3)
  
})
