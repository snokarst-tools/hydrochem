#' Piper Diagram Blueprint
#'
#' Generates an empty Piper diagram using ggplot2, including the base geometry,
#' optional grid lines, and labels. This template is intended for visualizing
#' hydrochemical data using Piper diagrams.
#'
#' @param include_NO3 Logical. If TRUE, includes nitrate (NO3) as an anion in the Piper diagram if present in the data. Default is FALSE.
#' @param grid_lines Logical. Whether to include dashed grid lines for the ternary fields and diamond. Default is `TRUE`.
#' @param axis_title_size Numeric. Text size for the ion group labels. Default is 3.5.
#' @param axis_tick_label_size Numeric. Text size for the tick value labels. Default is 3.
#' @param bold_label Logical. If TRUE, renders the ion group labels in bold. Default is FALSE.
#' 
#' @return A ggplot object representing an empty Piper diagram structure.
#' @noRd
#'
#' @import ggplot2

piper_blueprint <- function(include_NO3 = FALSE,
                            grid_lines = TRUE,
                            axis_title_size = 3.5,
                            axis_tick_label_size = 3,
                            bold_label = FALSE) {
  
  # Labels ------------------------------------------------------------------
  piper_labels <- list(
    "Mg^'2+'",
    "Na^'+'~+~K^'+'",
    "Ca^'2+'",
    ifelse(include_NO3, "Cl^'-'~+~NO[3]^'-'", "Cl^'-'"),
    "SO[4]^'2-'",
    "HCO[3]^'-'~+~CO[3]^'2-'",
    ifelse(include_NO3, "SO[4]^'2-'~+~Cl^'-'~+~NO[3]^'-'", "SO[4]^'2-'~+~Cl^'-'"),
    "Ca^'2+'~+~Mg^'2+'"
  )  
  
  if (bold_label) piper_labels <- lapply(piper_labels, \(x) paste0("bold(", x, ")"))
  
  # Structure ---------------------------------------------------------------
  grid1p1 <- data.frame(x1 = c(0, 20, 40, 60, 80, 100),
                        x2 = c(0, 10, 20, 30, 40, 50),
                        y1 = c(0, 0, 0, 0, 0, 0),
                        y2 = c(0, 17.3206, 34.6412, 51.9618, 69.2824, 86.603))
  
  grid1p2 <- data.frame(x1 = c(0, 20, 40, 60, 80, 100),
                        x2 = c(50, 60, 70, 80, 90, 100),
                        y1 = c(0, 0, 0, 0, 0, 0),
                        y2 = c(86.603, 69.2824, 51.9618, 34.6412, 17.3206, 0))
  
  grid1p3 <- data.frame(x1 = c(0, 10, 20, 30, 40, 50),
                        x2 = c(100, 90, 80, 70, 60, 50),
                        y1 = c(0, 17.3206, 34.6412, 51.9618, 69.2824, 86.603),
                        y2 = c(0, 17.3206, 34.6412, 51.9618, 69.2824, 86.603))
  
  grid2p1 <- grid1p1
  grid2p1$x1 <- grid2p1$x1 + 120
  grid2p1$x2 <- grid2p1$x2 + 120
  
  grid2p2 <- grid1p2
  grid2p2$x1 <- grid2p2$x1 + 120
  grid2p2$x2 <- grid2p2$x2 + 120
  
  grid2p3 <- grid1p3
  grid2p3$x1 <- grid2p3$x1 + 120
  grid2p3$x2 <- grid2p3$x2 + 120
  
  grid3p1 <- data.frame(x1 = c(110, 100, 90, 80, 70, 60),
                        y1 = c(17.3206, 34.6412, 51.9618, 69.2824, 86.603, 103.9236),
                        x2 = c(160, 150, 140, 130, 120, 110),
                        y2 = c(103.9236, 121.2442, 138.5648, 155.8854, 173.2060, 190.5266))
  
  grid3p2 <- data.frame(x1 = c(60, 70, 80, 90, 100, 110),
                        y1 = c(103.9236, 121.2442, 138.5648, 155.8854, 173.2060, 190.5266),
                        x2 = c(110, 120, 130, 140, 150, 160),
                        y2 = c(17.3206, 34.6412, 51.9618, 69.2824, 86.603, 103.9236))
  
  # Plot
  ggplot() +
    
    ## Left hand ternary plot
    geom_segment(aes(x = 0,y = 0, xend = 100, yend = 0)) +
    geom_segment(aes(x = 0,y = 0, xend = 50, yend = 86.603)) +
    geom_segment(aes(x = 50,y = 86.603, xend = 100, yend = 0)) +
    
    ## Right hand ternary plot
    geom_segment(aes(x = 120,y = 0, xend = 220, yend = 0)) +
    geom_segment(aes(x = 120,y = 0, xend = 170, yend = 86.603)) +
    geom_segment(aes(x = 170,y = 86.603, xend = 220, yend = 0)) +
    
    ## Upper diamond
    geom_segment(aes(x = 110,y = 190.5266, xend = 60, yend = 103.9236)) +
    geom_segment(aes(x = 110,y = 190.5266, xend = 160, yend = 103.9236)) +
    geom_segment(aes(x = 110,y = 17.3206, xend = 160, yend = 103.9236)) +
    geom_segment(aes(x = 110,y = 17.3206, xend = 60, yend = 103.9236)) +
    
    ## Add grid lines to the plots
    {if (grid_lines) list(
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid1p1,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid1p2,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid1p3,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid2p1,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid2p2,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid2p3,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid3p1,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid3p2,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50")
    )} +
    
    # Labels and grid values
    geom_text(aes(c(0, 20, 40, 60, 80, 100), c(-5, -5, -5, -5, -5, -5), label = c(100, 80, 60, 40, 20, 0)),
              size = axis_tick_label_size) +
    geom_text(aes(c(45, 35, 25, 15, 5, -5), grid1p2$y2, label = c(100, 80, 60, 40, 20, 0)),
              size = axis_tick_label_size) +
    coord_equal(ratio = 1) +
    geom_text(aes(c(225, 215, 205, 195, 185, 175), grid2p3$y2, label = c(0, 20, 40, 60, 80, 100)),
              size = axis_tick_label_size) +
    geom_text(aes(c(120, 140, 160, 180, 200, 220), c(-5, -5, -5, -5, -5, -5), label = c(0, 20, 40, 60, 80, 100)),
              size = axis_tick_label_size) +
    geom_text(aes(grid3p1$x1 - 5, grid3p1$y1 - 17.5, label = c(100, 80, 60, 40, 20, 0)),
              size = axis_tick_label_size) +
    geom_text(aes(c(grid3p1$x2[1:5], 104.5) + 5, c(grid3p1$y2[1:5], 193.5266), label = c(0, 20, 40, 60, 80, 100)),
              size = axis_tick_label_size) +
    geom_text(aes(grid3p2$x1[1:5] - 5, grid3p2$y1[1:5], label = c(0, 20, 40, 60, 80)),
              size = axis_tick_label_size) +
    geom_text(aes(grid3p2$x2 + 5, grid3p2$y2 - 17.5, label = c(100, 80, 60, 40, 20, 0)),
              size = axis_tick_label_size) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank()) +
    
    # Labels
    geom_text(aes(17, 50, label = piper_labels[[1]]),
              angle = 60,
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(85.5, 50, label = piper_labels[[2]]),
              angle = -60,
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(50, -10, label = piper_labels[[3]]),
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(170, -10, label = piper_labels[[4]]),
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(205, 50, label = piper_labels[[5]]),
              angle = -60,
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(136, 50, label = piper_labels[[6]]),
              angle = 60,
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(72.5, 150, label = piper_labels[[7]]),
              angle = 60,
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(147.5, 150, label = piper_labels[[8]]),
              angle = -60,
              size = axis_title_size,
              parse = TRUE)
}