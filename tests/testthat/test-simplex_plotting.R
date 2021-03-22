simdata$activity <- simdata$vigorous + simdata$moderate + simdata$light
#
# test_that("error if try to plot too many parts", {
#   expect_error(plot_density_ternary(data = simdata,
#                                     parts_to_plot = c("activity", "sedentary", "sleep", "moderate"),
#                                     n_bins = 10 # This argument specifies we want to use 10 bins
#
#   ))
# })
