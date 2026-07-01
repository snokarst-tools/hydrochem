#' Plot Piper Diagram from Hydrochemical Data
#'
#' Generates a Piper diagram from hydrochemical data. 
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' If carbonate (`CO3`) is also provided, its contribution is added
#' to bicarbonate after conversion to milliequivalents (`meq/L`). If `CO3` is absent, alkalinity is considered to be represented solely by bicarbonate (`HCO3`).
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param include_NO3 Logical. If `TRUE`, nitrate (`NO3`) is included. The `NO3` column must be present in the input data. Default is `FALSE`.
#' @param group Optional character string. Name of the column in `data` used to group points in the Piper diagram. If `NULL`, all points are plotted with the same style. Default is `NULL`.
#' @param group_custom Logical. If `TRUE`, enables manual aesthetics for each group defined in `group`. 
#' In this case, at least one of `color`, `fill`, `shape`, or `size` must be provided as a vector of length equal to the number of groups.
#' Any missing or incorrectly sized vectors will be automatically filled with default values (e.g., `"black"`, `"transparent"`, `21`, `2`).
#' If `FALSE` (default), all these aesthetics must be of length 1.
#' @param grid_lines Logical. Whether to include dashed grid lines for the ternary fields and diamond. Default is `TRUE`.
#' @param axis_title_size Numeric. Text size for the ion group labels. Default is 3.5.
#' @param axis_tick_label_size Numeric. Text size for the tick value labels. Default is 3.
#' @param bold_label Logical. If TRUE, renders the ion group labels in bold. Default is FALSE.
#' @param shape Integer or vector of integers. Shape(s) for plotting points. Must follow ggplot2 shape standards. Default is 21.
#' @param color Character. Outline color for the plotted points. Default is `"black"`.
#' @param fill Character. Fill color for the plotted points. Default is `"grey40"`.
#' @param size Numeric. Size of the plotted points. Default is 2.
#' @param plot_title Optional character string. Title of the plot. Default is NULL (no title displayed).
#' @param label Optional column name in `data` used to label points. Default is `NULL` (no labels).
#' @param label_size Numeric. Size of the label text. Default is `3`.
#' @param label_nudge_x Numeric. Horizontal offset of labels relative to points. Default is `0`.
#' @param label_nudge_y Numeric. Vertical offset of labels relative to points. Default is `0`.
#' @param legend.position Position of the legend. Accepts `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`, or a numeric vector of length 2 for relative coordinates, e.g., `c(1, 0.5)`. Default is `"bottom"`.
#' @param legend.title A character string for the title of the legend. If `NULL`, no legend title is displayed. Default is `NULL`.
#' @param bg_color Character. Background color of the plot area and panel. Accepts valid color names or hex codes (e.g., "red", "#FFFFFF"). Default is "#FFFFFF".
#'
#' @return A ggplot object displaying the Piper diagram.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' 
#' # Basic Piper plot without grouping
#' plot_piper(hc_data)
#'
#' # Piper plot with simple grouping
#' plot_piper(hc_data, group = "type")
#'
#' # Piper plot with custom aesthetics per group
#' plot_piper(
#'   hc_data,
#'   group = "type",
#'   group_custom = TRUE,
#'   fill = c("steelblue", "tomato", "darkgreen"),
#'   color = c("black", "black", "black"),
#'   shape = c(21, 22, 24),
#'   size  = c(2.5, 3, 3.5)
#' )
#'
#' @import ggplot2
#' @importFrom grDevices colors

plot_piper <- function(data, 
                       base_unit = c("mg/L", "mmol/L", "meq/L"),
                       include_NO3 = FALSE,
                       group = NULL,
                       group_custom = FALSE,
                       grid_lines = TRUE,
                       axis_title_size = 3.5,
                       axis_tick_label_size = 3,
                       bold_label = FALSE,
                       shape = 21,
                       color = "black",
                       fill = "grey40",
                       size = 2,
                       plot_title = NULL,
                       label = NULL,
                       label_size = 3,
                       label_nudge_x = 0,
                       label_nudge_y = 0,
                       legend.position = "bottom",
                       legend.title = NULL,
                       bg_color = "#FFFFFF") {
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  
  # label
  if (!is.null(label) && !label %in% names(data)) {
    stop("`label` column '", label, "' not found in data.", call. = FALSE)
  }
  
  # Define default ions used in Piper diagram -------------------------------
  cations = c("Ca", "Mg", "Na", "K")
  anions = c("Cl", "SO4", "HCO3")
  
  # Add CO3 to anions if present in the dataset 
  if ("CO3" %in% names(data)) anions <- c(anions, "CO3")
  
  # Add NO3 to anions if include_NO3=TRUE & present in the dataset 
  if (include_NO3) {
    if ("NO3" %in% names(data)) anions <- c(anions, "NO3")
    else if ("NO3_N" %in% names(data)) anions <- c(anions, "NO3_N")
    else stop("`include_NO3` is TRUE, but the 'NO3' or 'NO3_N' column is missing from the dataset.")
  }
  
  # Convert solutes mg/L to meq/L to percent --------------------------------
  piper_data <- transform_piper_data(data, cations = cations, anions = anions, base_unit = base_unit, group = group, label = label)
  
  # Plot --------------------------------------------------------------------
  all_groups <- unique(piper_data$observation)
  n_group <- length(all_groups)
  
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
  
  # Palette
  palette <- palette.colors()
  if (!is.null(group) & n_group > length(palette)) {
    all_colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    if (n_group > length(all_colors)) stop(paste0("Too much entities in the group (n=", n_group, ")"), call. = FALSE)
    palette <- sample(all_colors, n_group)
  }
  
  # Plot
  geom_point_layer <- function(data, group, group_custom, shape, color, fill, size) {
    if (is.null(group)) {
      geom_point(data = data, aes(x, y), shape = shape, color = color, fill = fill, size = size)
    } else if (!group_custom) {
      geom_point(data = data, aes(x, y, fill = observation), shape = shape, color = color, size = size)
    } else {
      geom_point(data = data, aes(x, y, fill = observation, shape = observation, color = observation, size = observation))
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
  
  x_vals <- piper_data$x
  y_vals <- piper_data$y
  
  piper_blueprint(include_NO3 = include_NO3,
                  grid_lines = grid_lines, 
                  axis_title_size = axis_title_size, 
                  axis_tick_label_size = axis_tick_label_size,
                  bold_label = bold_label) +
    geom_point_layer(piper_data, group, group_custom, shape, color, fill, size) +
    {if (!is.null(label)) ggplot2::geom_text(
      data = piper_data,
      aes(x = x, y = y, label = label),
      size = label_size,
      nudge_x = label_nudge_x * (max(x_vals, na.rm = TRUE) - min(x_vals, na.rm = TRUE)),
      nudge_y = label_nudge_y * (max(y_vals, na.rm = TRUE) - min(y_vals, na.rm = TRUE))
    )} +
    scale_list(group, group_custom, legend.title, palette, shape, color, size, fill) +
    {if (!is.null(plot_title)) ggtitle(plot_title)} +
    theme(legend.position = legend.position,
          legend.key = element_rect(fill = bg_color),
          legend.background = element_rect(fill = bg_color),
          panel.background = element_rect(fill = bg_color, color = bg_color),
          plot.background = element_rect(fill = bg_color, color = bg_color)) +
    {if (!is.null(legend.title)) theme(legend.title = element_text())}
  
}
