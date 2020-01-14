#' plot_density_ternary: Plot data density on a ternary plot
#'
#' Plot data density on a ternary plot
#'
#' @param data Data frame containing data to be plotted.
#' @param groups Name of variable in the data frame which identifies the groups to be plotted. This variable should be a factor variable.
#' @param parts_to_plot Names of the three variables in the data frame which are to be plotted on the ternary plot. Note they should be on the same scale (they don't need to be normalised to 1).
#' @param n_bins Number of bins to use on density plot.
#' @param mark_points Points should be the rows of a data frame with the elements of \code{parts_to_plot} as columns names. If a \code{groups} argument is given, it should also have a column for this (if the groups aren't relevant to the point in a certain row, this can be set as NA).
#' @param transparency Control the transparency of plots. Should be between 0 and 1.
#' @inheritParams plot_transfers
#' @return Plot showing density of data on ternary plot.
#' @examples plot_density_ternary(data = simdata,
#' parts_to_plot = c("combined_parts", "partD", "partE"),
#' n_bins = 10 # This argument specifies we want to use 10 bins (i.e. 10% of data lies between each pair of contours on the plot.)
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
      theme <- ggtern::theme(
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
        line = ggplot2::element_line(size = 1, colour = "black"),
        tern.axis.arrow = ggplot2::element_line(size = 2, colour = "black")
      )
    }


    # labels <- ggtern::Tlab(label = NA, labelarrow = paste(name1, "(%)")) + ggtern::Rlab(label = NA, labelarrow = paste(name2, "(%)")) + ggtern::Llab(label = NA, labelarrow = paste(name3, "(%)"))

    if (is.null(mark_points)) {
      if (is.null(groups)) {
        plot <-
          ggtern::ggtern(data = data[, parts_to_plot], ggplot2::aes_(x = data[, name1], y = data[, name2], z = data[, name3])) +
          ggtern::stat_density_tern(
            geom = 'polygon',
            bins = n_bins,
            weight = 3,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          ggtern::theme_showarrows() +
          ggtern::Tlab(label = "",
                       labelarrow = paste(name1, "(%)", "\n")) +
          ggtern::Rlab(label = "",
                       labelarrow = paste("\n", name2, "(%)")) +
          ggtern::Llab(label = "",
                       labelarrow = paste(name3, "(%)", "\n"))
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
          local_data <- data[data[, groups] == group, ]
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
              weight = 3,
              alpha = transparency
            )
        }
        plot <-
          plot + theme +
          ggtern::theme_showarrows() +
          ggtern::Tlab(label = "",
                       labelarrow = paste(name1, "(%)", "\n")) +
          ggtern::Rlab(label = "",
                       labelarrow = paste("\n", name2, "(%)")) +
          ggtern::Llab(label = "",
                       labelarrow = paste(name3, "(%)", "\n"))
      }

    }
    else{
      if (is.null(groups)) {
        plot <-
          ggtern::ggtern(data = data[, parts_to_plot], ggplot2::aes_(x = data[, name1], y = data[, name2], z = data[, name3])) +
          ggtern::stat_density_tern(
            data = data,
            geom = 'polygon',
            bins = n_bins,
            weight = 3,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          ggtern::theme_showarrows() +
          ggtern::Tlab(label = "",
                       labelarrow = paste(name1, "(%)", "\n")) +
          ggtern::Rlab(label = "",
                       labelarrow = paste("\n", name2, "(%)")) +
          ggtern::Llab(label = "",
                       labelarrow = paste(name3, "(%)", "\n")) +
          ggtern::geom_crosshair_tern(
            data = mark_points,
            mapping = ggplot2::aes_(x = mark_points[, name1], y = mark_points[, name2], z = mark_points[, name3])
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
          local_data <- data[data[, groups] == group, ]
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
              weight = 3,
              alpha = transparency
            )
        }
        plot <-
          plot + theme + ggtern::geom_crosshair_tern(
            data = mark_points,
            mapping = ggplot2::aes_(
              x = mark_points[, name1],
              y = mark_points[, name2],
              z = mark_points[, name3],
              color = mark_points[, groups]
            )
          ) + ggtern::theme_showarrows() +
          ggtern::Tlab(label = "",
                       labelarrow = paste(name1, "(%)", "\n")) +
          ggtern::Rlab(label = "",
                       labelarrow = paste("\n", name2, "(%)")) +
          ggtern::Llab(label = "",
                       labelarrow = paste(name3, "(%)", "\n"))

      }

    }

    return(plot)
  }
