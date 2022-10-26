# size/linewidth work in ggplot2 >= 3.4.0

    Code
      invisible(ggplot_gtable(ggplot_build(ggplot(dat, aes(x, y, colour = group)) +
        geom_borderline(size = 1))))
    Message <rlang_message>
      The use of `size` is deprecated since ggplot2 3.4.0, please use `linewidth` instead

