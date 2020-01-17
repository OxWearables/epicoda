# #' Save plots with desired quality.
# #'
# #' A wrapper for ggsave, to make saving with desired quality easier.
# #'
# #' ggsave or a routine such as tiff("filename") , plot, dev.off() can be used as usual to save plots; there is no need to employ this wrapper. However, this is intended to make saving higher quality images easier.
# #'
# #' @param plot Object output from a plotting function.
# #' @param dpi Resolution.
# #' @param filename Desired path to saved file.
# #' @param filetype Should be "tiff", "png", "pdf", "jpg" etc.
# #' @return Saves file with the desired size.
# #' @examples
# save_plot <- function(plot, dpi = 300, filename, filetype = "tiff"){
#   ggplot2::ggsave(filename = filename, plot, dpi = dpi, device = filetype )
#
# }
