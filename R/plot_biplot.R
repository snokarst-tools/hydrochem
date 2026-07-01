#' Plot Biplots from Hydrochemical Data
#'
#' Generates simple XY biplots from hydrochemical data using user-defined variable
#' pairs. Pairs are provided as character strings using the `"y ~ x"` syntax and
#' #' can include arithmetic expressions (e.g., `"Ca + Mg ~ Cl"` or `"Na / Cl ~ SO4"`). Input concentrations
#' may be provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param pairs Character vector of biplot definitions using the `"y ~ x"` syntax.
#'   Each element defines one panel. Arithmetic expressions are allowed on either side,
#'   e.g. `"Ca + Mg ~ Cl"`, `"Na * K ~ Cl"`, `"Na - K ~ Cl"`, or `"SO4 / Cl ~ NO3"`. Whitespace is ignored.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param to_unit Unit of the output ion concentrations. One of `"meq/L"`, `"mmol/L"`, or `"mg/L"`. Default is `"meq/L"`.
#' @param ncol Integer. Number of columns for the cowplot grid layout. Default is `NULL` (automatic layout).
#' @param group Optional column name in `data` used to group points. If `NULL`, a single style is used.
#' @param group_custom Logical. If `TRUE`, enables manual aesthetics per group defined in `group`.
#'   In that case, at least one of `color`, `fill`, `shape`, or `size` must be a vector of
#'   length equal to the number of groups. Missing or wrongly sized vectors are filled with
#'   defaults (`"black"`, `"transparent"`, `21`, `2`). If `FALSE` (default), these aesthetics
#'   must be length 1.
#' @param grid_lines Logical. Draw grid lines on panels. Default `TRUE`.
#' @param slope Numeric or numeric vector. Slope of a reference line drawn on the biplots.
#'   If length 1, the same slope is applied to all panels. If a vector, it must have
#'   the same length as `pairs`, with `NA` indicating that no line is drawn for a
#'   given panel. Must be provided together with `intercept`. Default is `NULL`
#'   (no reference line). Horizontal and vertical lines can be displayed using 0 and Inf, respectively.
#' @param intercept Numeric or numeric vector. Intercept of a reference line drawn on
#'   the biplots. If length 1, the same intercept is applied to all panels. If a vector,
#'   it must have the same length as `pairs`, with `NA` indicating that no line is drawn
#'   for a given panel. Must be provided together with `slope`. Default is `NULL`
#'   (no reference line).
#' @param log_x Logical or logical vector. If `TRUE`, the x-axis is displayed on a
#'   base-10 logarithmic scale. If length 1, the same setting is applied to all
#'   biplots. If a vector, it must have the same length as `pairs`, with `FALSE`
#'   indicating a linear scale for a given panel. Default is `FALSE`.
#' @param log_y Logical or logical vector. If `TRUE`, the y-axis is displayed on a
#'   base-10 logarithmic scale. If length 1, the same setting is applied to all
#'   biplots. If a vector, it must have the same length as `pairs`, with `FALSE`
#'   indicating a linear scale for a given panel. Default is `FALSE`.
#' @param xmin Numeric or numeric vector. Minimum value of the x-axis. If length 1, the same limit is applied to all biplots. If a vector, it must have the same length as `pairs`, with `NA` indicating that the limit is derived from the data for a given panel. Default is `NA`.
#' @param xmax Numeric or numeric vector. Maximum value of the x-axis. If length 1, the same limit is applied to all biplots. If a vector, it must have the same length as `pairs`, with `NA` indicating that the limit is derived from the data for a given panel. Default is `NA`.
#' @param ymin Numeric or numeric vector. Minimum value of the y-axis. If length 1, the same limit is applied to all biplots. If a vector, it must have the same length as `pairs`, with `NA` indicating that the limit is derived from the data for a given panel. Default is `NA`.
#' @param ymax Numeric or numeric vector. Maximum value of the y-axis. If length 1, the same limit is applied to all biplots. If a vector, it must have the same length as `pairs`, with `NA` indicating that the limit is derived from the data for a given panel. Default is `NA`.
#' @param slope_color Character. Color of the reference line. Default is `"black"`.
#' @param shape Integer or integer vector of point shapes (ggplot2 standards). Default `21`.
#' @param color Character. Point outline color. Default `"black"`.
#' @param fill Character. Point fill color. Default `"grey40"`.
#' @param size Numeric or numeric vector. Point size(s). Default `2`.
#' @param label Optional column name in `data` used to label points. Default is `NULL` (no labels).
#' @param label_size Numeric. Size of the label text. Default is `3`.
#' @param label_nudge_x Numeric. Horizontal offset of labels relative to points. Default is `0`.
#' @param label_nudge_y Numeric. Vertical offset of labels relative to points. Default is `0`.
#' @param legend.position Position of the legend. Accepts `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`, or a numeric vector of length 2 for relative coordinates, e.g., `c(1, 0.5)`. Default is `"bottom"`.
#' @param legend.title A character string for the title of the legend. If `NULL`, no legend title is displayed. Default is `NULL`.
#' @param bg_color Character. Background color of the plot area and panel. Accepts valid color names or hex codes (e.g., "red", "#FFFFFF"). Default is "#FFFFFF".
#'
#' @return A combined cowplot object containing the biplots (and legend if grouping is used).
#' @export
#'
#' @examples
#' data("data_qc", package = "hydrochem")
#' 
#' # Replace below detection limit values
#' data <- replace_bdl(data_qc)
#'
#' # Basic biplots
#' plot_biplot(
#'   data,
#'   pairs = c("Ca ~ Mg", "SO4 ~ Cl", "Cl + SO4 ~ NO3_N")
#' )
#'
#' # Grouped biplots
#' plot_biplot(
#'   data,
#'   pairs = c("Ca ~ HCO3", "Na ~ Cl"),
#'   group = "Cluster"
#' )
#'
#' # Custom aesthetics per group
#' plot_biplot(
#'   data,
#'   pairs = c("Ca ~ HCO3", "Na + K ~ Cl"),
#'   group = "Hydro_cond",
#'   group_custom = TRUE,
#'   fill = c("steelblue", "tomato", "darkgreen", "yellow"),
#'   color = c("black", "black", "black", "black"),
#'   shape = c(21, 22, 24, 25),
#'   size  = c(2.5, 3, 3.5, 4),
#'   ncol = 2
#' )
#' 
#' # Same reference line (1:1) on all biplots
#' plot_biplot(
#'   data,
#'   pairs = c("Ca ~ Ca", "Na ~ Na"),
#'   slope = 1,
#'   intercept = 0
#' )
#'
#' # Reference line on a single panel only
#' plot_biplot(
#'   data,
#'   pairs = c("Ca ~ Ca", "Ca + Mg ~ HCO3", "Cl ~ Na"),
#'   slope     = c(NA, NA, 1),
#'   intercept = c(NA, NA, 0)
#' )
#' 
#' # Log scale on x-axis for all biplots
#' plot_biplot(
#'   data,
#'   pairs = c("Ca ~ Mg", "Cl ~ Na"),
#'   log_x = TRUE
#' )
#'
#' # Log scale on selected panels only
#' plot_biplot(
#'   data,
#'   pairs = c("Ca ~ Mg", "Ca + Mg ~ HCO3", "Cl ~ Na"),
#'   log_x = c(FALSE, TRUE, FALSE),
#'   log_y = c(FALSE, FALSE, TRUE)
#' )
#'
#' @import ggplot2
#' @import data.table
#' @importFrom grDevices colors


plot_biplot <- function(data, 
                        pairs,
                        base_unit = c("mg/L", "mmol/L", "meq/L"),
                        to_unit = c("meq/L", "mmol/L", "mg/L"),
                        ncol = NULL,
                        group = NULL,
                        group_custom = FALSE,
                        grid_lines = TRUE,
                        slope = NA,
                        intercept = NA,
                        slope_color = "black",
                        xmin = NA,
                        xmax = NA,
                        ymin = NA,
                        ymax = NA,
                        log_x = FALSE,
                        log_y = FALSE,
                        shape = 21,
                        color = "black",
                        fill = "grey40",
                        size = 2,
                        label = NULL,
                        label_size = 3,
                        label_nudge_x = 0,
                        label_nudge_y = 0,
                        legend.position = "bottom",
                        legend.title = NULL,
                        bg_color = "#FFFFFF") {
  
  
  # Function for slope/hline/vline ------------------------------------------
  refline_layer <- function(slope, intercept, color) {
    if (is.na(slope) || is.na(intercept)) return(NULL)
    
    # horizontal
    if (isTRUE(all.equal(slope, 0))) {
      return(ggplot2::geom_hline(yintercept = intercept, color = color))
    }
    
    # vertical (allow Inf / -Inf)
    if (is.infinite(slope)) {
      return(ggplot2::geom_vline(xintercept = intercept, color = color))
    }
    
    ggplot2::geom_abline(slope = slope, intercept = intercept, color = color)
  }
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  to_unit <- match.arg(to_unit)
  params <- unique(trimws(unlist(strsplit(gsub("\\s+", "", pairs), "[~+*/-]"))))
  ions <- params[params %in% ions_meta$ion_r]
  split_pair <- function(s) trimws(strsplit(s, "~")[[1]])
  n <- length(pairs)
  
  # Check slope/intercept
  if (xor(all(is.na(slope)), all(is.na(intercept)))) {
    stop(
      "`slope` and `intercept` must be both NA or both provided.",
      call. = FALSE
    )
  }
  
  # If at least one line is requested
  if (!all(is.na(slope))) {
    
    # Scalar case → recycle to all plots
    if (length(slope) == 1 && length(intercept) == 1) {
      slope     <- rep(slope, n)
      intercept <- rep(intercept, n)
      
      # Vector case → strict length check
    } else {
      if (length(slope) != n || length(intercept) != n) {
        stop(
          "`slope` and `intercept` must have length 1 or the same length as `pairs` (",
          n, ").",
          call. = FALSE
        )
      }
    }
  }
  
  # log_x
  if (length(log_x) == 1) {
    log_x <- rep(log_x, n)
  } else if (length(log_x) != n) {
    stop("`log_x` must have length 1 or the same length as `pairs` (", n, ").",
         call. = FALSE)
  }
  
  # log_y
  if (length(log_y) == 1) {
    log_y <- rep(log_y, n)
  } else if (length(log_y) != n) {
    stop("`log_y` must have length 1 or the same length as `pairs` (", n, ").",
         call. = FALSE)
  }
  
  # label
  if (!is.null(label) && !label %in% names(data)) {
    stop("`label` column '", label, "' not found in data.", call. = FALSE)
  }
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Convert solutes ----------------------------------------------------
  data <- data |> 
    convert_unit(ions = ions, base_unit = base_unit, to_unit = to_unit, convert_to_species = TRUE)
  
  # xmin
  if (length(xmin) == 1) {
    xmin <- rep(xmin, n)
  } else if (length(xmin) != n) {
    stop("`xmin` must have length 1 or the same length as `pairs` (", n, ").",
         call. = FALSE)
  }
  
  # xmax
  if (length(xmax) == 1) {
    xmax <- rep(xmax, n)
  } else if (length(xmax) != n) {
    stop("`xmax` must have length 1 or the same length as `pairs` (", n, ").",
         call. = FALSE)
  }
  
  # ymin
  if (length(ymin) == 1) {
    ymin <- rep(ymin, n)
  } else if (length(ymin) != n) {
    stop("`ymin` must have length 1 or the same length as `pairs` (", n, ").",
         call. = FALSE)
  }
  
  # ymax
  if (length(ymax) == 1) {
    ymax <- rep(ymax, n)
  } else if (length(ymax) != n) {
    stop("`ymax` must have length 1 or the same length as `pairs` (", n, ").",
         call. = FALSE)
  }
  
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
  geom_point_layer <- function(group, group_custom, shape, color, fill, size) {
    if (is.null(group)) {
      geom_point(shape = shape, color = color, fill = fill, size = size)
    } else if (!group_custom) {
      geom_point(aes(fill = .data[[group]]), shape = shape, color = color, size = size)
    } else {
      geom_point(aes(fill = .data[[group]], shape = .data[[group]], color = .data[[group]], size = .data[[group]]))
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
  
  # Axis names
  axis_expr <- function(s, ions_meta, unit = to_unit) {
    tokens <- unlist(strsplit(gsub("\\s+", "", s), "(?=[+*/-])|(?<=[+*/-])", perl = TRUE))
    ion_idx <- !tokens %in% c("+", "*", "/", "-")
    if (any(is.na(match(tokens[ion_idx], ions_meta$ion_r)))) return(s)
    
    lab <- ions_meta$label[match(tokens[ion_idx], ions_meta$ion_r)]
    lab[is.na(lab)] <- tokens[ion_idx][is.na(lab)]
    tokens[ion_idx] <- lab
    
    expr <- paste(tokens, collapse = " ")
    parse(text = paste0(expr, " ~ (", unit, ")"))[[1]]
  }
  
  # Split ions
  split_ions <- function(s) {
    tokens <- unlist(strsplit(gsub("\\s+", "", s), "(?=[+*/-])|(?<=[+*/-])", perl = TRUE))
    tokens[!tokens %in% c("+", "*", "/", "-")]
  }
  
  # Get ion with _extension
  extension <- sub("/L$", "", to_unit)
  expr_meq <- function(s) gsub("\\b([A-Za-z][A-Za-z0-9_]*)\\b", paste0("\\1_", extension), s)
  
  plots <- lapply(seq_along(pairs), function(i) {
    xy <- split_pair(pairs[i])
    x_parts <- split_ions(xy[2])
    y_parts <- split_ions(xy[1])
    if (all(x_parts %in% ions_meta$ion_r)) x_expr <- expr_meq(xy[2]) else x_expr <- xy[2]
    if (all(y_parts %in% ions_meta$ion_r)) y_expr <- expr_meq(xy[1]) else y_expr <- xy[1]
    
    # xmin,xmax,ymin,ymax
    if (is.na(xmin[i])) xmin[i] <- min(eval(parse(text = x_expr), envir = data), na.rm = TRUE)
    if (is.na(xmax[i])) xmax[i] <- max(eval(parse(text = x_expr), envir = data), na.rm = TRUE)
    if (xmin[i] > xmax[i]) warning("xmin is greater than xmax", call. = FALSE)
    if (max(eval(parse(text = x_expr), envir = data), na.rm = TRUE) < xmin[i]) xmax[i] <- xmin[i]
    if (min(eval(parse(text = x_expr), envir = data), na.rm = TRUE) > xmax[i]) xmin[i] <- xmax[i]
    if (is.na(ymin[i])) ymin[i] <- min(eval(parse(text = y_expr), envir = data), na.rm = TRUE)
    if (is.na(ymax[i])) ymax[i] <- max(eval(parse(text = y_expr), envir = data), na.rm = TRUE)
    if (ymin[i] > ymax[i]) warning("ymin is greater than ymax", call. = FALSE)
    if (max(eval(parse(text = y_expr), envir = data), na.rm = TRUE) < ymin[i]) ymax[i] <- ymin[i]
    if (min(eval(parse(text = y_expr), envir = data), na.rm = TRUE) > ymax[i]) ymin[i] <- ymax[i]
    
    x_vals <- eval(parse(text = x_expr), envir = data)
    y_vals <- eval(parse(text = y_expr), envir = data)
    
    plot <- ggplot(data, aes(
      x = eval(parse(text = x_expr), envir = data),
      y = eval(parse(text = y_expr), envir = data)
    )) +
      {if (!is.na(slope[i])) refline_layer(slope[i], intercept[i], slope_color)} +
      geom_point_layer(group, group_custom, shape, color, fill, size) +
      {if (!is.null(label)) ggplot2::geom_text(
        aes(label = .data[[label]]),
        size = label_size,
        nudge_x = label_nudge_x * (max(x_vals, na.rm = TRUE) - min(x_vals, na.rm = TRUE) + 1),
        nudge_y = label_nudge_y * (max(y_vals, na.rm = TRUE) - min(y_vals, na.rm = TRUE) + 1)
      )} +
      scale_list(group, group_custom, legend.title, palette, shape, color, size, fill) +
      {if (log_x[i]) list(scale_x_continuous(trans = "log10"),
                          ggplot2::annotation_logticks(sides = "b"))} +
      {if (log_y[i]) list(scale_y_continuous(trans = "log10"),
                          ggplot2::annotation_logticks(sides = "l"))} +
      {if (!is.na(xmin[i])) coord_cartesian(xlim = c(xmin[i], xmax[i]), 
                                            ylim = c(ymin[i], ymax[i]))} +
      labs(
        x = axis_expr(xy[2], ions_meta),
        y = axis_expr(xy[1], ions_meta)
      ) +
      theme_bw() +
      theme(legend.position = legend.position,
            legend.key = element_rect(fill = bg_color),
            legend.background = element_rect(fill = bg_color),
            panel.background = element_rect(fill = bg_color, color = bg_color),
            plot.background = element_rect(fill = bg_color, color = bg_color)) +
      {if (!is.null(legend.title)) theme(legend.title = element_text())}
    
    if (log_x[i]) plot <- plot + theme(panel.grid.minor.x = element_blank())
    if (log_y[i]) plot <- plot + theme(panel.grid.minor.y = element_blank())
    
    plot
  })
  
  # Grid lines
  if (!grid_lines) {
    plots <- lapply(plots, function(p) {
      p + theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
    })
  }
  
  # Get legend
  legend <- cowplot::ggdraw(cowplot::get_legend(plots[[1]])) + 
    theme(plot.background = element_rect(fill = bg_color, color = bg_color))
  
  plots <- lapply(plots, function(p) {
    p + theme(legend.position = "none")
  })
  
  # Final
  combine <- cowplot::plot_grid(plotlist = plots, ncol = ncol, align = "vh")
  
  if (is.null(group)) {
    x <- combine
  } else {
    if (legend.position == "bottom") x <- cowplot::plot_grid(combine, legend, ncol = 1, rel_heights = c(10, 1))
    if (legend.position == "right") x <- cowplot::plot_grid(combine, legend, ncol = 2, rel_widths = c(10, 1))
    if (legend.position == "none") x <- combine
  }
  
  x
}