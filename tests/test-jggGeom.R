library(ggplot2)
library(JGGEarthquake)

# Previous task

df = eq_load_data("../extdata/signif.txt")

## geom_timeline test
test_geom_timeline <- geom_timeline()
test_that("geom_timeline has correct class", {
       expect_is(test_geom_timeline ,"ggproto")
})

## geom_timeline_label test
test_geom_timeline_label <- geom_timeline_label()
test_that("geom_timeline_label has correct class", {
   expect_is(test_geom_timeline_label ,"ggproto")
})

## eq_geom_timeline test
test_plot <- eq_geom_timeline(df)
test_that("result of eq_geom_timeline is a plot", {
    expect_is(test_plot,"ggplot")
})

## eq_geom_timeline test
test_plot_label <- eq_geom_timeline_label(df)
test_that("result of eq_geom_timeline_label is a plot", {
    expect_is(test_plot_label,"ggplot")
})

