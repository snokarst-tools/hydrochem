#' Save a plot Diagram to File
#'
#' Saves a plot diagram (or any ggplot object) to a file using `ggsave()`.
#'
#' @param filename Character. Path to the output file (e.g., `"piper_plot.png"`).
#' @param type Character. Diagram type used to choose default width/height if not
#'   supplied. One of `c("piper", "ternary", "collins", "stabler", "durov", "gibbs", schoeller", "stiff", "biplot")`.
#' @param plot A ggplot object to save. Default is the last plot (`last_plot()`).
#' @param dpi Resolution of the output image in dots per inch. Default is 300.
#' @param width,height Numeric. Output size. If `NULL`, defaults depend on `type`.
#' @param ... Additional arguments passed to `ggsave()`.
#'
#' @return Invisibly returns the filename.
#' @export
#'
#' @examples
#' p <- plot_piper(hc_data)
#' \donttest{
#'   save_plot(file.path(tempdir(), "piper.png"), type = "piper", plot = p)
#' }
#' 
#' p <- plot_stiff(hc_data[1:10, ], ncol = 5)
#' \donttest{
#'   if (interactive()) {
#'     save_plot(file.path(tempdir(), "stiff.png"), type = "stiff", plot = p, width = 10, height = 4)
#'   }
#' }

save_plot <- function(filename, type = NULL, plot = last_plot(), 
                      dpi = 300, width = NULL, height = NULL, ...) {
  
  # Supported diagram types
  types <- c("piper","ternary","collins","stabler","durov","gibbs","schoeller","stiff","biplot")
  
  # Require either a type or explicit size
  if (is.null(type) && is.null(width) && is.null(height)) {
    stop("Provide plot 'type' or specify at least one of 'width'/'height'.")
  }
  
  # Validate type if provided
  if (!is.null(type)) type <- match.arg(type, choices = types)
  
  # Default sizes per diagram (inches)
  plot_res <- list(
    piper     = c(6, 6),
    ternary   = c(6, 6),
    collins   = c(6, 6),
    stabler   = c(11, 6),
    durov     = c(11, 6),
    gibbs     = c(9, 5),
    schoeller = c(11, 6),
    stiff     = c(11, 6),
    biplot     = c(8, 6)
  )
  
  if (is.null(width))  width  <- plot_res[[type]][1]
  if (is.null(height)) height <- plot_res[[type]][2]
  
  ggsave(filename = filename, 
         plot = plot, 
         dpi = dpi, 
         width = width, 
         height = height, 
         ...)
  
  invisible(filename)
}