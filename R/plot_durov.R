#' Plot Durov Diagram from Hydrochemical Data
#'
#' Generates a Durov diagram from hydrochemical data. 
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' If carbonate (`CO3`) is also provided, its contribution is added
#' to bicarbonate after conversion to milliequivalents (`meq/L`). If `CO3` is absent, alkalinity is considered to be represented solely by bicarbonate (`HCO3`).
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param top_ions Character vector (length 3) of anions used on the top triangle,
#'   default `c("Cl","SO4","HCO3")`. If `"CO3"` exists, it is combined with `"HCO3"`
#'   unless `"HCO3"` is absent.
#' @param left_ions Character vector (length 3) of cations used on the left triangle,
#'   default `c("Mg","Na","Ca")`. If `"K"` is present, it is typically merged with `"Na"`.
#' @param tds_method TDS estimation for the Durov square: `"major"` (sum of major ions),
#'   `"major_si"` (major ions incl. silica if present), or `"all_ions"` (sum of all available ions).
#' @param group Optional column name in `data` used to group points. If `NULL`, a single style is used.
#' @param group_custom Logical. If `TRUE`, enables manual aesthetics per group defined in `group`.
#'   In that case, at least one of `color`, `fill`, `shape`, or `size` must be a vector of
#'   length equal to the number of groups. Missing or wrongly sized vectors are filled with
#'   defaults (`"black"`, `"transparent"`, `21`, `2`). If `FALSE` (default), these aesthetics
#'   must be length 1.
#' @param grid_lines Logical. Draw dashed grid lines on the ternaries and Durov square. Default `TRUE`.
#' @param plot_pH Logical. If `TRUE`, adds the optional pH scale plot below the main
#'   Durov diagram. Default `TRUE`.
#' @param plot_TDS Logical. If `TRUE`, adds the optional TDS scale plot to the right
#'   of the main Durov diagram. Default `TRUE`.
#' @param axis_title_size Numeric. Size for ion-group labels. Default `3.5`.
#' @param tds_log Logical. If `TRUE`, the TDS axis on the right panel is plotted
#'   on a base-10 logarithmic scale. Default `FALSE`.
#' @param axis_tick_label_size Numeric. Size for tick labels. Default `3`.
#' @param shape Integer or integer vector of point shapes (ggplot2 standards). Default `21`.
#' @param color Character. Point outline color. Default `"black"`.
#' @param fill Character. Point fill color. Default `"grey40"`.
#' @param size Numeric or numeric vector. Point size(s). Default `2`.
#' @param plot_title Optional character string. Title of the plot. Default is NULL (no title displayed).
#' @param label Optional column name in `data` used to label points. Default is `NULL` (no labels).
#' @param label_size Numeric. Size of the label text. Default is `3`.
#' @param label_nudge_x Numeric. Horizontal offset of labels relative to points. Default is `0`.
#' @param label_nudge_y Numeric. Vertical offset of labels relative to points. Default is `0`.
#' @param legend.position Position of the legend. Accepts `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`, or a numeric vector of length 2 for relative coordinates, e.g., `c(1, 0.5)`. Default is `"bottom"`.
#' @param legend.title A character string for the title of the legend. If `NULL`, no legend title is displayed. Default is `NULL`.
#' @param bg_color Character. Background color of the plot area and panel. Accepts valid color names or hex codes (e.g., "red", "#FFFFFF"). Default is "#FFFFFF".
#'
#' @return A `ggplot` object showing the Durov diagram.
#' @export
#'
#' @examples
#' data("data_qc", package = "hydrochem")
#' 
#' # Replace below detection limit values
#' data <- replace_bdl(data_qc)
#'
#' # Basic Durov plot
#' plot_durov(data)
#'
#' # Grouped Durov plot
#' plot_durov(data, group = "Cluster")
#'
#' # Custom aesthetics per group
#' plot_durov(
#'   data,
#'   group = "Hydro_cond",
#'   group_custom = TRUE,
#'   fill = c("steelblue", "tomato", "darkgreen", "yellow"),
#'   color = c("black", "black", "black", "black"),
#'   shape = c(21, 22, 24, 25),
#'   size  = c(2.5, 3, 3.5, 4)
#' )
#'
#' @import ggplot2
#' @importFrom grDevices colors

plot_durov <- function(data, 
                       base_unit = c("mg/L", "mmol/L", "meq/L"),
                       top_ions = c("Cl", "SO4", "HCO3"),
                       left_ions = c("Mg", "Na", "Ca"),
                       tds_method = c("major", "major_si", "all_ions"),
                       group = NULL,
                       group_custom = FALSE,
                       grid_lines = TRUE,
                       plot_pH = TRUE,
                       plot_TDS = TRUE,
                       tds_log = FALSE,
                       axis_title_size = 3.5,
                       axis_tick_label_size = 3,
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
  tds_method <- match.arg(tds_method)
  to_unit <- "meq/L"
  group_tmp <- NULL
  
  # label
  if (!is.null(label) && !label %in% names(data)) {
    stop("`label` column '", label, "' not found in data.", call. = FALSE)
  }
  
  t_labels <- list(
    ions_meta[ion_r == top_ions[1], label_durov],
    ions_meta[ion_r == top_ions[2], label_durov],
    ions_meta[ion_r == top_ions[3], label_durov]
  )  
  
  l_labels <- list(
    ions_meta[ion_r == left_ions[1], label_durov],
    ions_meta[ion_r == left_ions[2], label_durov],
    ions_meta[ion_r == left_ions[3], label_durov]
  )  
  
  # Add CO3 to anions if present in the dataset 
  if ("HCO3" %in% top_ions | "HCO3" %in% left_ions) {
    if ("CO3" %in% names(data)) data$HCO3 <- data$HCO3 + data$CO3
  }
  
  # Calculate percent
  data <- data |> 
    convert_unit(base_unit = base_unit, to_unit = "meq/L")
  
  current_unit <- sub("/L", "", to_unit)
  
  data$t_p <- rowSums(data[, paste0(top_ions, "_", current_unit), with = FALSE], na.rm = TRUE)
  data$l_p <- rowSums(data[, paste0(left_ions, "_", current_unit), with = FALSE], na.rm = TRUE)
  
  for (ion in top_ions) {
    col <- paste0(ion, "_", current_unit)
    data[[paste0(ion, "_", current_unit, "_p")]] <- 100 * data[[col]] / data$t_p
  }
  
  for (ion in left_ions) {
    col <- paste0(ion, "_", current_unit)
    data[[paste0(ion, "_", current_unit, "_p")]] <- 100 * data[[col]] / data$l_p
  }
  
  # Calculate TDS
  data$tds <- tds(data, method = tds_method)
  max_tds <- max(data$tds, na.rm = TRUE)
  min_tds <- min(data$tds, na.rm = TRUE)
  
  # Calculate pH
  if (!("pH" %in% names(data) && is.numeric(data$pH))) {
    stop("Column 'pH' missing or not numeric")
  }
  max_ph <- max(data$pH, na.rm = TRUE)
  min_ph <- min(data$pH, na.rm = TRUE)
  
  # Transform plot data
  ## Top
  first_col <- data$Cl_meq_p
  second_col <- data$SO4_meq_p
  if (is.null(group)) {
    group_tmp = rep(1:length(first_col), 1)
  } else {
    group_tmp = rep(data[[group]], 1)
  }
  
  if (is.null(label)) {
    label_tmp = rep(1:length(first_col), 1)
  } else {
    label_tmp = rep(data[[label]], 1)
  }
  
  x1 <- 100 * (1 - (first_col / 100) - (second_col / 200))
  y1 <- 100 + second_col * 0.86603
  
  top <- data.frame(observation = group_tmp, x = x1, y = y1, label = label_tmp)
  
  ## Left
  first_col <- data$Mg_meq_p
  second_col <- data$Na_meq_p
  if (is.null(group)) {
    group_tmp = rep(1:length(first_col), 1)
  } else {
    group_tmp = rep(data[[group]], 1)
  }
  
  if (is.null(label)) {
    label_tmp = rep(1:length(first_col), 1)
  } else {
    label_tmp = rep(data[[label]], 1)
  }
  
  x1 <- second_col * -0.86603
  y1 <- 100 * (1 - (first_col / 100) - (second_col / 200))
  
  left <- data.frame(observation = group_tmp, x = x1, y = y1, label = label_tmp)
  
  # Plot --------------------------------------------------------------------
  all_groups <- unique(top$observation)
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
  
  ## Structure top
  grid1p1 <- data.frame(x1 = c(0, 20, 40, 60, 80, 100),
                        x2 = c(0, 10, 20, 30, 40, 50),
                        y1 = c(100, 100, 100, 100, 100, 100),
                        y2 = c(100, 117.3206, 134.6412, 151.9618, 169.2824, 186.603))
  
  grid1p2 <- data.frame(x1 = c(0, 20, 40, 60, 80, 100),
                        x2 = c(50, 60, 70, 80, 90, 100),
                        y1 = c(100, 100, 100, 100, 100, 100),
                        y2 = c(186.603, 169.2824, 151.9618, 134.6412, 117.3206, 100))
  
  grid1p3 <- data.frame(x1 = c(0, 10, 20, 30, 40, 50),
                        x2 = c(100, 90, 80, 70, 60, 50),
                        y1 = c(100, 117.3206, 134.6412, 151.9618, 169.2824, 186.603),
                        y2 = c(100, 117.3206, 134.6412, 151.9618, 169.2824, 186.603))
  
  ## Structure left
  grid1p1l <- data.frame(y1 = c(0, 20, 40, 60, 80, 100),
                         y2 = c(0, 10, 20, 30, 40, 50),
                         x1 = c(0, 0, 0, 0, 0, 0),
                         x2 = c(0, -17.3206, -34.6412, -51.9618, -69.2824, -86.603))
  
  grid1p2l <- data.frame(y1 = c(0, 20, 40, 60, 80, 100),
                         y2 = c(50, 60, 70, 80, 90, 100),
                         x1 = c(0, 0, 0, 0, 0, 0),
                         x2 = c(-86.603, -69.2824, -51.9618, -34.6412, -17.3206, 0))
  
  grid1p3l <- data.frame(y1 = c(0, 10, 20, 30, 40, 50),
                         y2 = c(100, 90, 80, 70, 60, 50),
                         x1 = c(0, -17.3206, -34.6412, -51.9618, -69.2824, -86.603),
                         x2 = c(0, -17.3206, -34.6412, -51.9618, -69.2824, -86.603))
  
  ## Structure mid
  grid1p1m <- data.frame(x1 = c(20, 40, 60, 80, 0, 0, 0, 0),
                         x2 = c(20, 40, 60, 80, 100, 100, 100, 100),
                         y1 = c(0, 0, 0, 0, 20, 40, 60, 80),
                         y2 = c(100, 100, 100, 100, 20, 40, 60, 80))
  
  ## Text
  grid3p1 <- data.frame(x1 = c(110, 100, 90, 80, 70, 60),
                        y1 = c(117.3206, 134.6412, 151.9618, 169.2824, 186.603, 203.9236),
                        x2 = c(0, -17.3206, -34.6412, -51.9618, -69.2824, -86.603),
                        y2 = c(120, 110, 100, 90, 80, 70))
  
  # TDS grid lines
  if (!tds_log) {
    tds_ticks <- pretty(c(min_tds, max_tds), n = 5)
    min_tds_tick <- min(tds_ticks)
    max_tds_tick <- max(tds_ticks)
    tds_grid_x <- 100 + (tds_ticks - min_tds_tick) / (max_tds_tick - min_tds_tick) * 100
  } else {
    log_min <- log10(min_tds)
    log_max <- log10(max_tds)
    tds_ticks <- scales::log_breaks(n = 5)(c(min_tds, max_tds))
    min_tds_tick <- min(tds_ticks)
    max_tds_tick <- max(tds_ticks)
    tds_grid_x <- 100 + (log10(tds_ticks) - log_min) / (log_max - log_min) * 100
  }
  
  if (length(tds_ticks) > 2) {
    tds_ticks <- tds_ticks[-c(1, length(tds_ticks))]
    tds_grid_x <- tds_grid_x[-c(1, length(tds_grid_x))]
  } else {
    tds_ticks <- numeric(0)
    tds_grid_x <- numeric(0)
  }
  
  grid_tds <- data.frame(
    x = tds_grid_x,
    y1 = 0,
    y2 = 100,
    label = tds_ticks
  )
  
  # pH grid lines
  ph_ticks <- pretty(c(min_ph, max_ph), n = 5)
  min_ph_tick <- min(ph_ticks)
  max_ph_tick <- max(ph_ticks)
  ph_grid_y <- (ph_ticks - min_ph_tick) / (max_ph_tick - min_ph_tick) * (-50)
  ## Remove first and last tick
  if (length(ph_ticks) > 2) {
    ph_ticks <- ph_ticks[-c(1, length(ph_ticks))]
    ph_grid_y <- ph_grid_y[-c(1, length(ph_grid_y))]
  } else {
    ph_ticks <- numeric(0)   # no ticks to plot
    ph_grid_y <- numeric(0)   # no ticks to plot
  }
  grid_ph <- data.frame(
    y = ph_grid_y,
    x1 = 0,
    x2 = 100,
    label = ph_ticks
  )
  
  # Plot
  if (!tds_log) {
    data$tds_x <- (data$tds - min_tds_tick) / (max_tds_tick - min_tds_tick) * (200 - 100) + 100
  } else {
    log_min <- log10(min_tds_tick)
    log_max <- log10(max_tds_tick)
    data$tds_x <- (log10(data$tds) - log_min) / (log_max - log_min) * (200 - 100) + 100
  }
  
  geom_point_layer <- function(top, left, data, group, group_custom, shape, color, fill, size) {
    if (is.null(group)) {
      x <- list(
        ## Top
        geom_point(data = top, aes(x, y), shape = shape, color = color, fill = fill, size = size),
        ## Left
        geom_point(data = left, aes(x, y), shape = shape, color = color, fill = fill, size = size),
        ## Mid
        geom_point(data = data, aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
                                    100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200))),
                   shape = shape, color = color, fill = fill, size = size)
      )
      ## Right
      if (plot_TDS) x[[4]] <- geom_point(data = data, aes(data$tds_x,
                                                          100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200))),
                                         shape = shape, color = color, fill = fill, size = size)
      ## Bottom
      if (plot_pH) x[[5]] <- geom_point(data = data, aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
                                                         ((pH - min_ph_tick) / (max_ph_tick - min_ph_tick)) * (-50)),
                                        shape = shape, color = color, fill = fill, size = size)
      
      return(x)
      
    } else if (!group_custom) {
      x <- list(
        ## Top
        geom_point(data = top, aes(x, y, fill = observation), shape = shape, color = color, size = size),
        ## Left
        geom_point(data = left, aes(x, y, fill = observation), shape = shape, color = color, size = size),
        ## Mid
        geom_point(data = data, aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
                                    100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200)),
                                    fill = .data[[group]]),
                   shape = shape, color = color, size = size)
      )
      ## Right
      if (plot_TDS) x[[4]] <- geom_point(data = data, aes(data$tds_x, 
                                                          100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200)),
                                                          fill = .data[[group]]),
                                         shape = shape, color = color, size = size)
      ## Bottom
      if (plot_pH) x[[5]] <- geom_point(data = data, aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
                                                         ((pH - min_ph_tick) / (max_ph_tick - min_ph_tick)) * (-50),
                                                         fill = .data[[group]]),
                                        shape = shape, color = color, size = size)
      
      return(x)
      
    } else {
      x <- list(
        ## Top
        geom_point(data = top, aes(x, y, fill = observation, shape = observation, color = observation, size = observation)),
        ## Left
        geom_point(data = left, aes(x, y, fill = observation, shape = observation, color = observation, size = observation)),
        ## Mid
        geom_point(data = data, aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
                                    100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200)),
                                    fill = .data[[group]],
                                    shape = .data[[group]], 
                                    color = .data[[group]], 
                                    size = .data[[group]]))
      )
      ## Right
      if (plot_TDS) x[[4]] <- geom_point(data = data, aes(data$tds_x, 
                                                          100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200)),
                                                          fill = .data[[group]],
                                                          shape = .data[[group]], 
                                                          color = .data[[group]], 
                                                          size = .data[[group]]))
      ## Bottom
      if (plot_pH) x[[5]] <- geom_point(data = data, aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
                                                         ((pH - min_ph_tick) / (max_ph_tick - min_ph_tick)) * (-50),
                                                         fill = .data[[group]],
                                                         shape = .data[[group]], 
                                                         color = .data[[group]], 
                                                         size = .data[[group]]))
      
      return(x)
      
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
  
  data_x <- 100 * (1 - (data$Cl_meq_p / 100) - (data$SO4_meq_p / 200))
  data_y <- 100 * (1 - (data$Mg_meq_p / 100) - (data$Na_meq_p / 200))
  
  ggplot() +
    ## Upper ternary plot
    geom_segment(aes(x = 0, y = 100, xend = 100, yend = 100)) +
    geom_segment(aes(x = 0, y = 100, xend = 50, yend = 186.603)) +
    geom_segment(aes(x = 50, y = 186.603, xend = 100, yend = 100)) +
    ## Left ternary plot
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 100)) +
    geom_segment(aes(x = 0, y = 0, xend = -86.603, yend = 50)) +
    geom_segment(aes(x = -86.603, y = 50, xend = 0, yend = 100)) +
    ## Middle plot
    geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0)) +
    geom_segment(aes(x = 100, y = 0, xend = 100, yend = 100)) +
    
    ## Right plot
    {if (plot_TDS) list(geom_segment(aes(x = 100, y = 0, xend = 200, yend = 0)),
                        geom_segment(aes(x = 100, y = 100, xend = 200, yend = 100)),
                        geom_segment(aes(x = 200, y = 0, xend = 200, yend = 100)))} +
    ## Bottom plot
    {if (plot_pH) list(geom_segment(aes(x = 0, y = 0, xend = 0, yend = -50)),
                       geom_segment(aes(x = 100, y = 0, xend = 100, yend = -50)),
                       geom_segment(aes(x = 0, y = -50, xend = 100, yend = -50)))} +
    
    # Grid lines
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
                   data = grid1p1l,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid1p2l,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid1p3l,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50"),
      geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2),
                   data = grid1p1m,
                   linetype = "dashed",
                   linewidth = 0.25,
                   colour = "grey50")
    )} +
    # Right-panel vertical TDS grid lines
    {if (grid_lines & plot_TDS) geom_segment(data = grid_tds,
                                             aes(x = x, xend = x, y = y1, yend = y2),
                                             linetype = "dashed",
                                             linewidth = 0.25,
                                             colour = "grey50")} +
    # Bottom-panel horizontal ph grid lines
    {if (grid_lines & plot_pH) geom_segment(data = grid_ph,
                                            aes(x = x1, xend = x2, y = y, yend = y),
                                            linetype = "dashed",
                                            linewidth = 0.25,
                                            colour = "grey50")} +
    
    # Data
    geom_point_layer(top, left, data, group, group_custom, shape, color, fill, size) +
    scale_list(group, group_custom, legend.title, palette, shape, color, size, fill) +
    
    # Label
    {if (!is.null(label)) ggplot2::geom_text(
      data = data,
      aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
          100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200)), 
          label = .data[[label]]),
      size = label_size,
      nudge_x = label_nudge_x * (max(data_x, na.rm = TRUE) - min(data_x, na.rm = TRUE) + 1),
      nudge_y = label_nudge_y * (max(data_y, na.rm = TRUE) - min(data_y, na.rm = TRUE) + 1)
    )} +
    {if (!is.null(label)) ggplot2::geom_text(
      data = top,
      aes(x, y, label = label),
      size = label_size,
      nudge_x = label_nudge_x * (max(data_x, na.rm = TRUE) - min(data_x, na.rm = TRUE) + 1),
      nudge_y = label_nudge_y * (max(data_y, na.rm = TRUE) - min(data_y, na.rm = TRUE) + 1)
    )} +
    {if (!is.null(label)) ggplot2::geom_text(
      data = left,
      aes(x, y, label = label),
      size = label_size,
      nudge_x = label_nudge_x * (max(data_x, na.rm = TRUE) - min(data_x, na.rm = TRUE) + 1),
      nudge_y = label_nudge_y * (max(data_y, na.rm = TRUE) - min(data_y, na.rm = TRUE) + 1)
    )} +
    {if (!is.null(label) & plot_TDS) ggplot2::geom_text(
      data = data,
      aes(tds_x, 
          100 * (1 - (Mg_meq_p / 100) - (Na_meq_p / 200)), 
          label = .data[[label]]),
      size = label_size,
      nudge_x = label_nudge_x * (max(data_x, na.rm = TRUE) - min(data_x, na.rm = TRUE) + 1),
      nudge_y = label_nudge_y * (max(data_y, na.rm = TRUE) - min(data_y, na.rm = TRUE) + 1)
    )} +
    {if (!is.null(label) & plot_pH) ggplot2::geom_text(
      data = data,
      aes(100 * (1 - (Cl_meq_p / 100) - (SO4_meq_p / 200)), 
          ((pH - min_ph_tick) / (max_ph_tick - min_ph_tick)) * (-50), 
          label = .data[[label]]),
      size = label_size,
      nudge_x = label_nudge_x * (max(data_x, na.rm = TRUE) - min(data_x, na.rm = TRUE) + 1),
      nudge_y = label_nudge_y * (max(data_y, na.rm = TRUE) - min(data_y, na.rm = TRUE) + 1)
    )} +
    
    # Theme
    coord_equal(ratio = 1) +
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
    {if (plot_pH) geom_text(aes(50, -60, label = t_labels[[1]]),
                            size = axis_title_size,
                            parse = TRUE)
      else geom_text(aes(50, -25, label = t_labels[[1]]),
                     size = axis_title_size,
                     parse = TRUE)} +
    geom_text(aes(17, 150, label = t_labels[[2]]),
              angle = 60,
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(85.5, 150, label = t_labels[[3]]),
              angle = -60,
              size = axis_title_size,
              parse = TRUE) +
    {if (plot_TDS) geom_text(aes(220, 50, label = l_labels[[1]]),
                             size = axis_title_size,
                             parse = TRUE)
      else geom_text(aes(130, 50, label = l_labels[[1]]),
                     size = axis_title_size,
                     parse = TRUE)} +
    geom_text(aes(-50, 17, label = l_labels[[2]]),
              angle = 330,
              size = axis_title_size,
              parse = TRUE) +
    geom_text(aes(-50, 85.5, label = l_labels[[3]]),
              angle = 30,
              size = axis_title_size,
              parse = TRUE) +
    {if (plot_TDS) geom_text(aes(150, c(-15), label = "TDS~(mg/L)"),
                             angle = 0,
                             size = axis_title_size,
                             parse = TRUE)} +
    {if (plot_pH) geom_text(aes(c(-15), -25, label = "pH"),
                            angle = 0,
                            size = axis_title_size,
                            parse = TRUE)} +
    
    # Axis text
    {if (plot_pH) geom_text(aes(c(0, 20, 40, 60, 80, 100), c(-55, -55, -55, -55, -55, -55), label = c(100, 80, 60, 40, 20, 0)),
                            size = axis_tick_label_size)
      else geom_text(aes(c(0, 20, 40, 60, 80, 100), c(-15, -15, -15, -15, -15, -15), label = c(100, 80, 60, 40, 20, 0)),
                     size = axis_tick_label_size)} +
    geom_text(aes(c(45, 35, 25, 15, 5, -5), grid1p2$y2, label = c(100, 80, 60, 40, 20, 0)),
              size = axis_tick_label_size) +
    geom_text(aes((grid3p1$x1 - 5)[2:6], (grid3p1$y1 - 17.5)[2:6], label = c(80, 60, 40, 20, 0)),
              size = axis_tick_label_size) +
    {if (plot_TDS) geom_text(aes(c(205, 205, 205, 205, 205, 205), c(0, 20, 40, 60, 80, 100), label = c(100, 80, 60, 40, 20, 0)),
                             size = axis_tick_label_size)
      else geom_text(aes(c(115, 115, 115, 115, 115, 115), c(0, 20, 40, 60, 80, 100), label = c(100, 80, 60, 40, 20, 0)),
                     size = axis_tick_label_size)} +
    geom_text(aes(grid3p1$x2 - 5, grid3p1$y2 - 17.5, label = c(100, 80, 60, 40, 20, 0)),
              size = axis_tick_label_size) +
    geom_text(aes(grid1p2l$x2[1:5], c(45, 35, 25, 15, 5), label = c(100, 80, 60, 40, 20)),
              size = axis_tick_label_size) +
    # {if (plot_pH) geom_text(aes(c(103, 103), c(-45, -5), label = c(round(min_ph), round(max_ph))),
    #                         size = axis_tick_label_size)} +
    {if (plot_pH) geom_text(aes(105, grid_ph$y, label = grid_ph$label),
                            size = axis_tick_label_size)} +
    {if (plot_pH) geom_text(aes(-5, grid_ph$y, label = grid_ph$label),
                            size = axis_tick_label_size)} +
    # {if (plot_TDS) geom_text(aes(c(105, 195), c(-3, -3), label = c(round(min_tds), round(max_tds))),
    #                          size = axis_tick_label_size)} +
    {if (plot_TDS) geom_text(aes(grid_tds$x, -4, label = grid_tds$label),
                             size = axis_tick_label_size)} +
    {if (plot_TDS) geom_text(aes(grid_tds$x, 104, label = grid_tds$label),
                             size = axis_tick_label_size)} +
    
    # Axis ticks
    {if (plot_pH) geom_segment(aes(x = c(0, 20, 40, 60, 80, 100), 
                                   y = c(-50, -50, -50, -50, -50, -50),
                                   xend = c(0, 20, 40, 60, 80, 100), 
                                   yend = c(-51.5, -51.5, -51.5, -51.5, -51.5, -51.5)))} +
    {if (plot_TDS) geom_segment(aes(x = c(200, 200, 200, 200, 200, 200), 
                                    y = c(0, 20, 40, 60, 80, 100),
                                    xend = c(201.5, 201.5, 201.5, 201.5, 201.5, 201.5), 
                                    yend = c(0, 20, 40, 60, 80, 100)))}
  
}


