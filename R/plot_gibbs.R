#' Plot Gibbs Diagram from Hydrochemical Data
#'
#' Generates a Gibbs diagram from hydrochemical data. 
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param tds_method Character. Method to compute total dissolved solids (TDS). `"major"` uses only Na, Ca, Cl, HCO3; `"major_si"` includes additional major ions (e.g. Mg, SO4); `"all_ions"` uses all columns. Default is `"major"`.
#' @param show_guide Logical. If `TRUE`, displays dashed reference lines approximating domain boundaries and transitions 
#' within the Gibbs diagram (e.g., evaporation dominance, rock weathering, precipitation influence). Default is `TRUE`.
#' @param show_text Logical. If TRUE, labels for the dominance zones are displayed on the plot.
#' @param group Optional character string. Column name in `data` used to group points. If `NULL`, all points are styled the same. Default is `NULL`.
#' @param group_custom Logical. If `TRUE`, allows custom aesthetics (color, fill, shape, size) per group. Must supply vectors of appropriate length. If `FALSE`, all aesthetics must be scalars. Default is `FALSE`.
#' @param base_size Numeric. Base font size for the plot. Default is `12`.
#' @param shape Integer or vector of integers. Shape(s) for plotting points (ggplot2 standard). Default is `21`.
#' @param color Character. Outline color of points. Default is `"black"`.
#' @param fill Character. Fill color(s) for points. Default is `"grey40"`.
#' @param size Numeric. Size of points. Default is `2`.
#' @param plot_title Optional character string. Plot title. Default is `NULL`.
#' @param label Optional column name in `data` used to label points. Default is `NULL` (no labels).
#' @param label_size Numeric. Size of the label text. Default is `3`.
#' @param label_nudge_x Numeric. Horizontal offset of labels relative to points. Default is `0`.
#' @param label_nudge_y Numeric. Vertical offset of labels relative to points. Default is `0`.
#' @param legend.position Position of the legend. Accepts `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`, or a numeric vector of length 2 for relative coordinates, e.g., `c(1, 0.5)`. Default is `"bottom"`.
#' @param legend.title A character string for the title of the legend. If `NULL`, no legend title is displayed. Default is `NULL`.
#' @param bg_color Character. Background color of the plot area and panel. Accepts valid color names or hex codes (e.g., "red", "#FFFFFF"). Default is "#FFFFFF".
#'
#' @return A `ggplot` object showing the Gibbs diagram.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#'
#' # Basic Gibbs diagram
#' plot_gibbs(hc_data)
#'
#' # With grouping
#' plot_gibbs(hc_data, group = "type")
#'
#' # With custom fill and shape
#' plot_gibbs(
#'   hc_data,
#'   group = "type",
#'   group_custom = TRUE,
#'   fill = c("skyblue", "tomato", "green"),
#'   shape = c(21, 22, 23)
#' )
#'
#' # With all ions included in TDS computation
#' plot_gibbs(hc_data, tds_method = "all_ions")
#'
#' @import ggplot2
#' @import data.table
#' @importFrom cowplot ggdraw get_legend plot_grid

plot_gibbs <- function(data,
                       base_unit = c("mg/L", "mmol/L", "meq/L"),
                       tds_method = c("major", "major_si", "all_ions"),
                       show_guide = TRUE,
                       show_text = FALSE,
                       group = NULL,
                       group_custom = FALSE,
                       base_size = 12,
                       shape = 21,
                       color = "black",
                       fill = "grey40",
                       size = 2,
                       plot_title = NULL,
                       label = NULL,
                       label_size = 3,
                       label_nudge_x = 0,
                       label_nudge_y = 0,
                       legend.position = c("bottom", "right", "none"),
                       legend.title = NULL,
                       bg_color = "#FFFFFF") {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  tds_method <- match.arg(tds_method)
  legend.position <- match.arg(legend.position)
  log_range <- c(1, 10, 100, 1000, 10000, 100000)
  log_range_label <- c("1", "10", "100", "1000", "10000", "100000")
  
  # label
  if (!is.null(label) && !label %in% names(data)) {
    stop("`label` column '", label, "' not found in data.", call. = FALSE)
  }
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Get ions ----------------------------------------------------------------
  datacol <- names(data)
  cations <- datacol[datacol %in% ions_meta$ion_r[ions_meta$type == "cation"]]
  anions <- datacol[datacol %in% ions_meta$ion_r[ions_meta$type == "anion"]]
  ions <- c(cations, anions)
  
  # Convert solutes mg/L ----------------------------------------------------
  data <- data |> 
    convert_unit(ions = ions, base_unit = base_unit, to_unit = "mg/L")
  
  # Calculate TDS -----------------------------------------------------------
  data$tds <- tds(data, base_unit = "mg/L", method = tds_method)
  
  # Calculate Gibbs cols ----------------------------------------------------
  data$gibbs1 <- data$Na_mg / (data$Na_mg + data$Ca_mg)
  data$gibbs2 <- data$Cl_mg / (data$Cl_mg + data$HCO3_mg)
  
  # Plot --------------------------------------------------------------------
  # Palette
  palette <- palette.colors()
  
  if (!is.null(group)) {
    all_groups <- unique(data[[group]])
    n_group <- length(all_groups)
    
    if (n_group > length(palette)) {
      all_colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
      if (n_group > length(all_colors)) stop(paste0("Too much entities in the group (n=", n_group, ")"), call. = FALSE)
      palette <- sample(all_colors, n_group)
    }
  }
  
  # Validate grouping and aesthetics
  if (group_custom && is.null(group)) {
    stop("`group_custom = TRUE` requires a non-NULL `group`. Set `group` to a column name in the data.", call. = FALSE)
  }
  
  if (group_custom) {
    check_color <- length(color) == n_group
    check_fill  <- length(fill)  == n_group
    check_shape <- length(shape) == n_group
    check_size  <- length(size)  == n_group
    if (!any(check_color, check_fill, check_shape, check_size)) {
      stop("When 'group_custom' is TRUE, at least one of 'color', 'fill', 'shape', and 'size' must have the same length as the number of groups (", n_group, ").",
           call. = FALSE)
    }
    if (!check_color) {
      message("Color vector length does not match number of groups. Using default 'black'.")
      color <- rep("black", n_group)
    }
    if (!check_fill) {
      message("Fill vector length does not match number of groups. Using default 'transparent'.")
      fill <- rep("transparent", n_group)
    }
    if (!check_shape) {
      message("Shape vector length does not match number of groups. Using default 21.")
      shape <- rep(21, n_group)
    }
    if (!check_size) {
      message("Size vector length does not match number of groups. Using default 2.")
      size <- rep(2, n_group)
    }
  } else if (!is.null(group)) {
    dif_aes <- (shape == 21 & color == "black" & fill == "grey40" & size == 2)
    if (any(lengths(list(fill, shape, color, size)) > 1)) {
      stop("When using grouping (`group` is not NULL) without `group_custom = TRUE`, aesthetics 'fill', 'shape', 'color', and 'size' must each be of length 1. ",
           "To specify group-level aesthetics, set `group_custom = TRUE` and provide vectors of length equal to the number of groups (", n_group, ").",
           call. = FALSE)
    }
    if (fill != "grey40") message("Cannot modify fill as it is the grouping aesthetic. To modify filling color, using `group_custom = TRUE`.")
  }
  
  # Plot
  geom_point_layer <- function(data, col, group, group_custom, shape, color, fill, size) {
    if (is.null(group)) {
      geom_point(data = data, aes(.data[[col]], tds), shape = shape, color = color, fill = fill, size = size)
    } else if (!group_custom) {
      geom_point(data = data, aes(.data[[col]], tds, fill = .data[[group]]), shape = shape, color = color, size = size)
    } else {
      geom_point(data = data, aes(.data[[col]], tds, fill = .data[[group]], shape = .data[[group]], color = .data[[group]], size = .data[[group]]))
    }
  }
  
  scale_list <- function(group, group_custom, legend.title, palette, shape, color, size, fill) {
    if (is.null(group)) return(NULL)
    if (!group_custom) {
      return(scale_fill_manual(name = legend.title, values = palette, na.value = palette[n_group]))
    } else {
      return(list(
        scale_fill_manual(name = legend.title, values = fill, na.value = fill[n_group]),
        scale_shape_manual(name = legend.title, values = shape, na.value = shape[n_group]),
        scale_color_manual(name = legend.title, values = color, na.value = color[n_group]),
        scale_size_manual(name = legend.title, values = size, na.value = size[n_group])
      ))
    }
  }
  
  gibbs1_val <- data$gibbs1
  gibbs2_val <- data$gibbs2
  tds_val <- data$tds
  
  gibbs1 <- ggplot() +
    # Limites des zones (approximation des lignes en pointillés)
    {if (show_guide) list(
      annotate("segment", x = 0, y = 1000, xend = 0.8, yend = 65000, linetype = "dashed"),
      annotate("curve", x = 0.8, y = 65000, xend = 0.93, yend = 40000, linetype = "dashed", curvature = -0.55),
      annotate("curve", x = 0.93, y = 40000, xend = 0.82, yend = 3000, linetype = "dashed", curvature = -0.15),
      annotate("segment", x = 0.55, y = 600, xend = 0.82, yend = 3000, linetype = "dashed"),
      annotate("curve", x = 0.55, y = 600, xend = 0.55, yend = 150, linetype = "dashed", curvature = 0.55),
      annotate("segment", x = 0.55, y = 150, xend = 0.82, yend = 30, linetype = "dashed"),
      annotate("curve", x = 0.82, y = 30, xend = 0.93, yend = 2, linetype = "dashed", curvature = -0.2),
      annotate("curve", x = 0.87, y = 1.5, xend = 0.93, yend = 2, linetype = "dashed", curvature = 0.3),
      annotate("segment", x = 0, y = 100, xend = 0.87, yend = 1.5, linetype = "dashed")
    )} +
    # Giss text
    {if (show_text) list(
      annotate("text", x = 0.1, y = 100, label = "Rock Dominance", hjust  = 0),
      annotate("text", x = 0.5, y = 5000, label = "Evaporation Dominance", hjust  = 0),
      annotate("text", x = 0.5, y = 20, label = "Precipitation Dominance", hjust  = 0)
    )} +
    
    geom_point_layer(data, "gibbs1", group, group_custom, shape, color, fill, size) +
    scale_list(group, group_custom, legend.title, palette, shape, color, size, fill) +
    {if (!is.null(label)) ggplot2::geom_text(
      data = data,
      aes(x = gibbs1, y = tds, label = .data[[label]]),
      size = label_size,
      nudge_x = label_nudge_x * (max(gibbs1_val, na.rm = TRUE) - min(gibbs1_val, na.rm = TRUE) + 1),
      nudge_y = label_nudge_y * (max(tds_val, na.rm = TRUE) - min(tds_val, na.rm = TRUE) + 1)
    )} +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(
      trans = "log10",
      breaks = log_range,
      labels = log_range_label,
      expand = c(0, 0)
    ) +
    ggplot2::annotation_logticks(sides = "l") +
    coord_cartesian(xlim = c(0, 1), ylim = c(1, 100000)) +
    xlab(expression(Na^"+" / (Na^"+" + Ca^"2+"))) + 
    ylab("TDS [mg/L]") +
    {if (!is.null(plot_title)) ggtitle(plot_title)} +
    theme_bw(base_size = base_size) +
    theme(legend.position = legend.position,
          legend.key = element_rect(fill = bg_color),
          legend.background = element_rect(fill = bg_color),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = bg_color, color = bg_color),
          plot.background = element_rect(fill = bg_color, color = bg_color)) +
    {if (!is.null(legend.title)) theme(legend.title = element_text())}
  
  gibbs2 <- ggplot() +
    # Gibbs lines
    {if (show_guide) list(
      annotate("segment", x = 0, y = 1000, xend = 0.8, yend = 65000, linetype = "dashed"),
      annotate("curve", x = 0.8, y = 65000, xend = 0.93, yend = 40000, linetype = "dashed", curvature = -0.55),
      annotate("curve", x = 0.93, y = 40000, xend = 0.82, yend = 3000, linetype = "dashed", curvature = -0.15),
      annotate("segment", x = 0.55, y = 600, xend = 0.82, yend = 3000, linetype = "dashed"),
      annotate("curve", x = 0.55, y = 600, xend = 0.55, yend = 150, linetype = "dashed", curvature = 0.55),
      annotate("segment", x = 0.55, y = 150, xend = 0.82, yend = 30, linetype = "dashed"),
      annotate("curve", x = 0.82, y = 30, xend = 0.93, yend = 2, linetype = "dashed", curvature = -0.2),
      annotate("curve", x = 0.87, y = 1.5, xend = 0.93, yend = 2, linetype = "dashed", curvature = 0.3),
      annotate("segment", x = 0, y = 100, xend = 0.87, yend = 1.5, linetype = "dashed")
    )} +
    # Giss text
    {if (show_text) list(
      annotate("text", x = 0.1, y = 100, label = "Rock Dominance", hjust  = 0),
      annotate("text", x = 0.5, y = 5000, label = "Evaporation Dominance", hjust  = 0),
      annotate("text", x = 0.5, y = 20, label = "Precipitation Dominance", hjust  = 0)
    )} +
    
    geom_point_layer(data, "gibbs2", group, group_custom, shape, color, fill, size) +
    scale_list(group, group_custom, legend.title, palette, shape, color, size, fill) +
    {if (!is.null(label)) ggplot2::geom_text(
      data = data,
      aes(x = gibbs2, y = tds, label = .data[[label]]),
      size = label_size,
      nudge_x = label_nudge_x * (max(gibbs2_val, na.rm = TRUE) - min(gibbs2_val, na.rm = TRUE) + 1),
      nudge_y = label_nudge_y * (max(tds_val, na.rm = TRUE) - min(tds_val, na.rm = TRUE) + 1)
    )} +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(
      trans = "log10",
      breaks = log_range,
      labels = log_range_label,
      expand = c(0, 0)
    ) +
    ggplot2::annotation_logticks(sides = "l") +
    coord_cartesian(xlim = c(0, 1), ylim = c(1, 100000)) +
    xlab(expression(Cl^"-" / (Cl^"-" + HCO[3]^"-"))) +
    ylab("TDS [mg/L]") +
    {if (!is.null(plot_title)) ggtitle(plot_title)} +
    theme_bw(base_size = base_size) +
    theme(legend.position = "none",
          legend.key = element_rect(fill = bg_color),
          legend.background = element_rect(fill = bg_color),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = bg_color, color = bg_color),
          plot.background = element_rect(fill = bg_color, color = bg_color)) +
    {if (!is.null(legend.title)) theme(legend.title = element_text())}
  
  legend <- cowplot::ggdraw(cowplot::get_legend(gibbs1)) + 
    theme(plot.background = element_rect(fill = bg_color, color = bg_color))
  
  gibbs1 <- gibbs1 + theme(legend.position = "none")
  
  combine <- cowplot::plot_grid(gibbs1, gibbs2, ncol = 2)
  
  if (legend.position == "bottom") x <- cowplot::plot_grid(combine, legend, ncol = 1, rel_heights = c(10, 1))
  if (legend.position == "right") x <- cowplot::plot_grid(combine, legend, ncol = 2, rel_widths = c(10, 1))
  if (legend.position == "none") x <- combine
  
  x
}
