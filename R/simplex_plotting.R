#' @title
#' plot_density_ternary: Plot data density on a ternary plot
#'
#' @description
#' Plot data density on a ternary plot.
#'
#' @details
#' This is a wrapper for `ggtern`.
#'
#' @param data Data frame containing data to be plotted.
#' @param groups If plotting groups separately, name of the variable in the data
#' frame which identifies the groups to be plotted. This variable should be a
#' factor variable.
#' @param parts_to_plot Names of the three variables in the data frame which are
#' to be plotted on the ternary plot. Note they should be on the same scale
#' (they don't need to be normalised to 1).
#' @param n_bins Number of bins to use on density plot.
#' @param mark_points Points should be the rows of a data frame with the
#' elements of `parts_to_plot` as columns names. If a `groups` argument is
#' given, it should also have a column for this (if the groups aren't relevant
#' to the point in a certain row, this can be set as NA).
#' @param transparency Control the transparency of plots. Should be between 0
#' and 1.
#' @param suppress_legend Suppress legend on plot. May be used when combining
#' plots.
#' @inheritParams plot_transfers
#' 
#' @return
#' Plot showing density of data on ternary plot.
#' 
#' @importFrom ggplot2 aes_ element_line element_rect element_text labs theme
#' @importFrom ggtern geom_crosshair_tern ggtern stat_confidence_tern stat_density_tern theme_showarrows
#' 
#' @examples
#' simdata$activity <- simdata$vigorous + simdata$moderate + simdata$light
#'
#' plot_density_ternary(
#'   data = simdata,
#'   parts_to_plot = c("activity", "sedentary", "sleep"),
#'   n_bins = 10 # This argument specifies we want to use 10 bins
#'   # (i.e. 9 equi-density contours will be plotted between the minimum and maximum
#'   # probability density.) Default is n_bins = 5.
#' )

#' @export
plot_density_ternary <-
  function(data,
           groups = NULL,
           parts_to_plot = NULL,
           n_bins = 5,
           mark_points = NULL,
           theme = NULL,
           transparency = 0.2,
           suppress_legend = FALSE) {
    if (length(parts_to_plot) != 3) {
      stop("parts_to_plot should have names of exactly three parts to plot")
    }
    name1 <- parts_to_plot[1]
    name2 <- parts_to_plot[2]
    name3 <- parts_to_plot[3]

    if (is.null(theme)) {
      theme <- theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "grey75"),
        text = element_text(
          size = 15,
          face = 2,
          colour = "black"
        ),
        axis.text = element_text(
          size = 18,
          face = 2,
          colour = "black"
        ),
        line = element_line(size = 1, colour = "black")
      )
    }

    if (suppress_legend){
      theme <- theme + theme(legend.position = "none")
    }




    if (is.null(mark_points)) {
      if (is.null(groups)) {
        plot <-
          ggtern(data = data[, parts_to_plot], aes_(x = data[, name1], y = data[, name2], z = data[, name3])) +
          stat_density_tern(
            geom = 'polygon',
           bins = n_bins,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
        theme_showarrows() +
        labs( x       = "",
                xarrow  = paste(name1, "(%)", "\n"),
                y       = "",
                yarrow  = paste(name2, "(%)", "\n"),
                z       = "",
                 zarrow  =  paste("\n", name3, "(%)"))
      }
      else{
        plot <-
          ggtern(
            data = data[, c(parts_to_plot, groups)],
            aes_(
              x = data[, name1],
              y = data[, name2],
              z = data[, name3],
              color = data[, groups]
            )
          )
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + stat_density_tern(
              data = local_data,
              geom = 'polygon',
              aes_(
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
          theme_showarrows() +
          labs(x       = "",
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
          ggtern(data = data[, parts_to_plot], aes_(x = data[, name1], y = data[, name2], z = data[, name3])) +
          stat_density_tern(
            data = data[, parts_to_plot],
            geom = 'polygon',
            bins = n_bins,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          theme_showarrows() +
          labs(x       = "",
               xarrow  = paste(name1, "(%)", "\n"),
               y       = "",
               yarrow  = paste(name2, "(%)", "\n"),
               z       = "",
               zarrow  =  paste("\n", name3, "(%)")) +
          geom_crosshair_tern(
            data = mark_points,
            mapping = aes_(x = mark_points[, name1], y = mark_points[,  name2], z = mark_points[, name3])
          )

      }
      else{
        plot <-
          ggtern(data = data[, c(parts_to_plot, groups)],
                         aes_(
                           x = data[, name1],
                           y = data[, name2],
                           z = data[, name3],
                           color = data[, groups]
                         ))
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + stat_density_tern(
              data = local_data,
              geom = 'polygon',
              aes_(
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
          plot + theme + geom_crosshair_tern(
            data = mark_points,
            mapping = aes_(
              x = mark_points[, name1],
              y = mark_points[,  name2],
              z = mark_points[, name3],
              color = mark_points[, groups]
            )
          ) + theme_showarrows() +
          labs(x       = "",
               xarrow  = paste(name1, "(%)", "\n"),
               y       = "",
               yarrow  = paste(name2, "(%)", "\n"),
               z       = "",
               zarrow  =  paste("\n", name3, "(%)"))

      }

    }

    return(plot)
  }


#' @title
#' plot_confidence_region_ternary: Plot predictive confidence regions on a
#' ternary plot
#'
#' @description 
#' Plot predictive confidence regions. Please note these wrap the `ggtern`
#' function `geom_confidence_region` but are actually prediction regions (or
#' predictive confidence regions): regions in which a future data point is
#' expected to fall with the given probability (rather than a confidence region
#' for a particular statistic).
#'
#' @details
#' This is a wrapper for `ggtern`.
#'
#' @param probs Sequence of probabilities to plot preidction regions for.
#' @inheritParams plot_density_ternary
#' 
#' @return
#' Plot showing prediction regions for data on ternary plot.
#' 
#' @importFrom ggplot2 aes_ element_line element_rect element_text labs theme
#' @importFrom ggtern geom_crosshair_tern ggtern stat_confidence_tern theme_showarrows
#' 
#' @examples
#' simdata$activity <- simdata$vigorous + simdata$moderate + simdata$light
#'
#' plot_confidence_region_ternary(
#'   data = simdata,
#'   parts_to_plot = c("activity", "sedentary", "sleep"),
#'   probs = c(0.25, 0.5, 0.75) # This argument specifies
#'   # we want to plot 25%, 50% and 75% prediction regions.
#' )

#' @export
plot_confidence_region_ternary <-
  function(data,
           groups = NULL,
           parts_to_plot = NULL,
           probs = c(0.5, 0.9, 0.95),
           mark_points = NULL,
           theme = NULL,
           transparency = 0.2,
           suppress_legend = FALSE) {
    if (length(parts_to_plot) != 3) {
      stop("parts_to_plot should have names of exactly three parts to plot")
    }
    name1 <- parts_to_plot[1]
    name2 <- parts_to_plot[2]
    name3 <- parts_to_plot[3]

    if (is.null(theme)) {
      theme <- theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "grey75"),
        text = element_text(
          size = 15,
          face = 2,
          colour = "black"
        ),
        axis.text = element_text(
          size = 18,
          face = 2,
          colour = "black"
        ),
        line = element_line(size = 1, colour = "black")
      )
    }
    if (suppress_legend){
      theme <- theme + theme(legend.position = "none")
    }



    if (is.null(mark_points)) {
      if (is.null(groups)) {
        plot <-
          ggtern(data = data[, parts_to_plot], aes_(x = data[, name1], y = data[,name2], z = data[, name3])) +
          stat_confidence_tern(
            geom = 'polygon',
            breaks = probs,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          theme_showarrows() +
          labs(x       = "",
               xarrow  = paste(name1, "(%)", "\n"),
               y       = "",
               yarrow  = paste(name2, "(%)", "\n"),
               z       = "",
               zarrow  =  paste("\n", name3, "(%)"))
      }
      else{
        plot <-
          ggtern(data = data[, c(parts_to_plot, groups)],
                         aes_(
                           x = data[, name1],
                           y = data[,  name2],
                           z = data[, name3],
                           color = data[, groups]
                         ))
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + stat_confidence_tern(
              data = local_data,
              geom = 'polygon',
              aes_(
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
          theme_showarrows() +
          labs(x       = "",
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
          ggtern(data = data[, parts_to_plot], aes_(x = data[, name1], y = data[, name2], z = data[, name3])) +
          stat_confidence_tern(
            data = data[, parts_to_plot],
            geom = 'polygon',
            breaks = probs,
            alpha = transparency,
            colour = "black"
          ) +
          theme +
          theme_showarrows() +
          labs(x       = "",
               xarrow  = paste(name1, "(%)", "\n"),
               y       = "",
               yarrow  = paste(name2, "(%)", "\n"),
               z       = "",
               zarrow  =  paste("\n", name3, "(%)")) +
          geom_crosshair_tern(
            data = mark_points,
            mapping = aes_(x = mark_points[, name1], y = mark_points[,  name2], z = mark_points[, name3])
          )

      }
      else{
        plot <-
          ggtern(data = data[, c(parts_to_plot, groups)],
                         aes_(
                           x = data[, name1],
                           y = data[, name2],
                           z = data[, name3],
                           color = data[, groups]
                         ))
        for (group in levels(data[, groups])) {
          local_data <- data[data[, groups] == group, c(parts_to_plot, groups)]
          plot <-
            plot + stat_confidence_tern(
              data = local_data,
              geom = 'polygon',
              aes_(
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
          plot + theme + geom_crosshair_tern(
            data = mark_points,
            mapping = aes_(
              x = mark_points[, name1],
              y = mark_points[,  name2],
              z = mark_points[, name3],
              color = mark_points[, groups]
            )
          ) + theme_showarrows() +
          labs(x       = "",
               xarrow  = paste(name1, "(%)", "\n"),
               y       = "",
               yarrow  = paste(name2, "(%)", "\n"),
               z       = "",
               zarrow  =  paste("\n", name3, "(%)"))

      }

    }

    return(plot)
  }
