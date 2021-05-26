#' plot_density_ternary: Plot data density on a ternary plot
#'
#' Plot data density on a ternary plot.
#'
#' This is a wrapper for \code{ggtern}.
#'
#' @param data Data frame containing data to be plotted.
#' @param groups If plotting groups separately, name of the variable in the data frame which identifies the groups to be plotted. This variable should be a factor variable.
#' @param parts_to_plot Names of the three variables in the data frame which are to be plotted on the ternary plot. Note they should be on the same scale (they don't need to be normalised to 1).
#' @param n_bins Number of bins to use on density plot.
#' @param mark_points Points should be the rows of a data frame with the elements of \code{parts_to_plot} as columns names. If a \code{groups} argument is given, it should also have a column for this (if the groups aren't relevant to the point in a certain row, this can be set as NA).
#' @param transparency Control the transparency of plots. Should be between 0 and 1.
#' @inheritParams plot_transfers
#' @return Plot showing density of data on ternary plot.
#' @examples
#' simdata$activity <- simdata$vigorous + simdata$moderate + simdata$light
#'
#' plot_density_ternary(data = simdata,
#' parts_to_plot = c("activity", "sedentary", "sleep"),
#' n_bins = 10 # This argument specifies we want to use 10 bins
#' # (i.e. 9 equi-density contours will be plotted between the minimum and maximum
#' # probability density.) Default is n_bins = 5.
#' )
#' @export
plot_density_ternary <-
  function(data,
           groups = NULL,
           parts_to_plot = NULL,
           n_bins = 5,
           mark_points = NULL,
           theme = NULL,
           transparency = 0.2) {
    if (length(parts_to_plot) != 3) {
      stop("parts_to_plot should have names of exactly three parts to plot")
    }
    name1 <- parts_to_plot[1]
    name2 <- parts_to_plot[2]
    name3 <- parts_to_plot[3]

    if (is.null(theme)) {
      theme <- ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "grey75"),
        text = ggplot2::element_text(
          size = 15,
          face = 2,
          colour = "black"
        ),
        axis.text = ggplot2::element_text(
          size = 18,
          face = 2,
          colour = "black"
        ),
        line = ggplot2::element_line(size = 1, colour = "black")
      )
    }



    if (is.null(mark_points)) {
      if (is.null(groups)) {
        plot <-
          ggtern::ggtern(data = data[, parts_to_plot], ggplot2::aes_(x = data[, name1], y = data[,name2], z = data[, name3])) +
          ggtern::stat_density_tern(
            geom = 'polygon',
           bins = n_bins,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
        ggtern::theme_showarrows() +
        ggplot2::labs( x       = "",
                xarrow  = paste(name1, "(%)", "\n"),
                y       = "",
                yarrow  = paste(name2, "(%)", "\n"),
                z       = "",
                 zarrow  =  paste("\n", name3, "(%)"))
      }
      else{
        plot <-
          ggtern::ggtern(data = data[, c(parts_to_plot, groups)],
                         ggplot2::aes_(
                           x = data[, name1],
                           y = data[,  name2],
                           z = data[, name3],
                           color = data[, groups]
                         ))
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + ggtern::stat_density_tern(
              data = local_data,
              geom = 'polygon',
              ggplot2::aes_(
                x = local_data[, name1],
                y = local_data[, name2],
                z = local_data[, name3],
                color = local_data[, groups]
              ),
              bins = n_bins,
              alpha = transparency
            )
        }
        plot <-
          plot + theme +
          ggtern::theme_showarrows() +
          ggplot2::labs( x       = "",
                         xarrow  = paste(name1, "(%)", "\n"),
                         y       = "",
                         yarrow  = paste(name2, "(%)", "\n"),
                         z       = "",
                         zarrow  =  paste("\n", name3, "(%)"))
      }

    }
    else{
      if (is.null(groups)) {
        plot <-
          ggtern::ggtern(data = data[, parts_to_plot], ggplot2::aes_(x = data[, name1], y = data[, name2], z = data[, name3])) +
          ggtern::stat_density_tern(
            data = data[, parts_to_plot],
            geom = 'polygon',
            bins = n_bins,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          ggtern::theme_showarrows() +
          ggplot2::labs( x       = "",
                         xarrow  = paste(name1, "(%)", "\n"),
                         y       = "",
                         yarrow  = paste(name2, "(%)", "\n"),
                         z       = "",
                         zarrow  =  paste("\n", name3, "(%)")) +
          ggtern::geom_crosshair_tern(
            data = mark_points,
            mapping = ggplot2::aes_(x = mark_points[, name1], y = mark_points[,  name2], z = mark_points[, name3])
          )

      }
      else{
        plot <-
          ggtern::ggtern(data = data[, c(parts_to_plot, groups)],
                         ggplot2::aes_(
                           x = data[, name1],
                           y = data[, name2],
                           z = data[, name3],
                           color = data[, groups]
                         ))
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + ggtern::stat_density_tern(
              data = local_data,
              geom = 'polygon',
              ggplot2::aes_(
                x = local_data[, name1],
                y = local_data[, "\n", name2],
                z = local_data[, name3],
                color = local_data[, groups]
              ),
              bins = n_bins,
              alpha = transparency
            )
        }
        plot <-
          plot + theme + ggtern::geom_crosshair_tern(
            data = mark_points,
            mapping = ggplot2::aes_(
              x = mark_points[, name1],
              y = mark_points[,  name2],
              z = mark_points[, name3],
              color = mark_points[, groups]
            )
          ) + ggtern::theme_showarrows() +
          ggplot2::labs( x       = "",
                         xarrow  = paste(name1, "(%)", "\n"),
                         y       = "",
                         yarrow  = paste(name2, "(%)", "\n"),
                         z       = "",
                         zarrow  =  paste("\n", name3, "(%)"))

      }

    }

    return(plot)
  }


#' plot_confidence_region_ternary: Plot confidence regions on a ternary plot
#'
#' Plot confidence regions.
#'
#' This is a wrapper for \code{ggtern}.
#'
#' @param data Data frame containing data to be plotted.
#' @param groups If plotting groups separately, name of the variable in the data frame which identifies the groups to be plotted. This variable should be a factor variable.
#' @param parts_to_plot Names of the three variables in the data frame which are to be plotted on the ternary plot. Note they should be on the same scale (they don't need to be normalised to 1).
#' @param probs Sequence of probabilities to plot confidence regions for.
#' @param mark_points Points should be the rows of a data frame with the elements of \code{parts_to_plot} as columns names. If a \code{groups} argument is given, it should also have a column for this (if the groups aren't relevant to the point in a certain row, this can be set as NA).
#' @param transparency Control the transparency of plots. Should be between 0 and 1.
#' @inheritParams plot_transfers
#' @return Plot showing confidence regions for data on ternary plot.
#' @examples
#' simdata$activity <- simdata$vigorous + simdata$moderate + simdata$light
#'
#' plot_confidence_region_ternary(data = simdata,
#' parts_to_plot = c("activity", "sedentary", "sleep"),
#' probs = c(0.25, 0.5, 0.75) # This argument specifies we want to plot 25%, 50% and 75% confidence regions.
#' )
#' @export
plot_confidence_region_ternary <-
  function(data,
           groups = NULL,
           parts_to_plot = NULL,
           probs = c(0.5, 0.9, 0.95),
           mark_points = NULL,
           theme = NULL,
           transparency = 0.2) {
    if (length(parts_to_plot) != 3) {
      stop("parts_to_plot should have names of exactly three parts to plot")
    }
    name1 <- parts_to_plot[1]
    name2 <- parts_to_plot[2]
    name3 <- parts_to_plot[3]

    if (is.null(theme)) {
      theme <- ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "grey75"),
        text = ggplot2::element_text(
          size = 15,
          face = 2,
          colour = "black"
        ),
        axis.text = ggplot2::element_text(
          size = 18,
          face = 2,
          colour = "black"
        ),
        line = ggplot2::element_line(size = 1, colour = "black")
      )
    }



    if (is.null(mark_points)) {
      if (is.null(groups)) {
        plot <-
          ggtern::ggtern(data = data[, parts_to_plot], ggplot2::aes_(x = data[, name1], y = data[,name2], z = data[, name3])) +
          ggtern::stat_confidence_tern(
            geom = 'polygon',
            breaks = probs,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          ggtern::theme_showarrows() +
          ggplot2::labs( x       = "",
                         xarrow  = paste(name1, "(%)", "\n"),
                         y       = "",
                         yarrow  = paste(name2, "(%)", "\n"),
                         z       = "",
                         zarrow  =  paste("\n", name3, "(%)"))
      }
      else{
        plot <-
          ggtern::ggtern(data = data[, c(parts_to_plot, groups)],
                         ggplot2::aes_(
                           x = data[, name1],
                           y = data[,  name2],
                           z = data[, name3],
                           color = data[, groups]
                         ))
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + ggtern::stat_density_tern(
              data = local_data,
              geom = 'polygon',
              ggplot2::aes_(
                x = local_data[, name1],
                y = local_data[, name2],
                z = local_data[, name3],
                color = local_data[, groups]
              ),
              breaks = probs,
              alpha = transparency
            )
        }
        plot <-
          plot + theme +
          ggtern::theme_showarrows() +
          ggplot2::labs( x       = "",
                         xarrow  = paste(name1, "(%)", "\n"),
                         y       = "",
                         yarrow  = paste(name2, "(%)", "\n"),
                         z       = "",
                         zarrow  =  paste("\n", name3, "(%)"))
      }

    }
    else{
      if (is.null(groups)) {
        plot <-
          ggtern::ggtern(data = data[, parts_to_plot], ggplot2::aes_(x = data[, name1], y = data[, name2], z = data[, name3])) +
          ggtern::stat_density_tern(
            data = data[, parts_to_plot],
            geom = 'polygon',
            breaks = probs,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          ggtern::theme_showarrows() +
          ggplot2::labs( x       = "",
                         xarrow  = paste(name1, "(%)", "\n"),
                         y       = "",
                         yarrow  = paste(name2, "(%)", "\n"),
                         z       = "",
                         zarrow  =  paste("\n", name3, "(%)")) +
          ggtern::geom_crosshair_tern(
            data = mark_points,
            mapping = ggplot2::aes_(x = mark_points[, name1], y = mark_points[,  name2], z = mark_points[, name3])
          )

      }
      else{
        plot <-
          ggtern::ggtern(data = data[, c(parts_to_plot, groups)],
                         ggplot2::aes_(
                           x = data[, name1],
                           y = data[, name2],
                           z = data[, name3],
                           color = data[, groups]
                         ))
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + ggtern::stat_density_tern(
              data = local_data,
              geom = 'polygon',
              ggplot2::aes_(
                x = local_data[, name1],
                y = local_data[, "\n", name2],
                z = local_data[, name3],
                color = local_data[, groups]
              ),
              breaks = probs,
              alpha = transparency
            )
        }
        plot <-
          plot + theme + ggtern::geom_crosshair_tern(
            data = mark_points,
            mapping = ggplot2::aes_(
              x = mark_points[, name1],
              y = mark_points[,  name2],
              z = mark_points[, name3],
              color = mark_points[, groups]
            )
          ) + ggtern::theme_showarrows() +
          ggplot2::labs( x       = "",
                         xarrow  = paste(name1, "(%)", "\n"),
                         y       = "",
                         yarrow  = paste(name2, "(%)", "\n"),
                         z       = "",
                         zarrow  =  paste("\n", name3, "(%)"))

      }

    }

    return(plot)
  }
