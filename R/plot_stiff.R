#' Plot Stiff Diagrams from Hydrochemical Data
#'
#' Generates Stiff diagrams from hydrochemical data.
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param include_NO3 Character or `NULL`. If set to `"Cl"` or `"SO4"`, nitrate (`NO3`) is included in the Stiff diagram by being added to the specified anion. Default is `NULL`.
#' @param idcol Character or `NULL`. Column name used to identify and facet individual diagrams.
#' If not provided, a sequential ID is created. If provided, it is converted to a factor with levels
#' ordered by first appearance in the data, ensuring facet order matches data order. Default is `NULL`.
#' @param group Optional character string. Name of the column used for coloring or filling groups. Default is `NULL`.
#' @param group_custom Logical. If `TRUE`, enables manual aesthetics for each group defined in `group`. Requires vectors `color`, `fill` (each of length equal to the number of groups). Default is `FALSE`.
#' @param nrow,ncol Integers. Number of rows and columns for the facet layout. Default is `NULL` (automatic layout).
#' @param scales Character. Facet scaling behavior. Must be `"fixed"` or `"free_x"`. Default is `"fixed"`.
#' @param facet_dir Character. Facet layout direction. Either `"v"` for vertical or `"h"` for horizontal. Default is `"v"`.
#' @param xlab Character. Label for the x-axis. Set `NULL` for no label. Default is `"meq/L"`.
#' @param base_size Numeric. Base font size for the plot. Default is `12`.
#' @param bold_label Logical. If `TRUE`, displays y-axis group labels in bold. Default is `FALSE`.
#' @param color Character or vector. Outline color(s) for polygons. If `group_custom = TRUE`, must be a vector. Default is `"black"`.
#' @param fill Character or vector. Fill color(s) for polygons. If `group_custom = TRUE`, must be a vector. Default is `"grey40"`.
#' @param axis_title_size Numeric. Font size for facet strip labels. Default is `14`.
#' @param label_border_color Character. Border color of strip labels. Use `"transparent"` for no border. Default is `"transparent"`.
#' @param plot_title Optional character string. Title of the plot. Default is NULL (no title displayed).
#' @param legend.position Position of the legend. Accepts `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`, or a numeric vector of length 2 for relative coordinates, e.g., `c(1, 0.5)`. Default is `"bottom"`.
#' @param legend.title A character string for the title of the legend. If `NULL`, no legend title is displayed. Default is `NULL`.
#' @param show_label Logical. If `TRUE`, displays strip labels in each facet. Default is `TRUE`.
#' @param bg_color Character. Background color of the plot area and panel. Accepts valid color names or hex codes (e.g., "red", "#FFFFFF"). Default is "#FFFFFF".
#'
#' @return A `ggplot` object representing the Stiff diagram(s).
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' hc_data <- hc_data[1:20, ] # keep first 20 rows
#' 
#' # Basic Stiff plot
#' plot_stiff(hc_data)
#'
#' # Stiff plot with sample IDs
#' plot_stiff(hc_data, idcol = "id")
#'
#' # Stiff plot for selected samples only
#' sel_data <- hc_data[hc_data$id %in% c("id_1", "id_5", "id_20"), ]
#' plot_stiff(sel_data)
#' plot_stiff(sel_data, group = "type") # with grouping
#'
#' # Stiff plot with controlled facet order
#' sel_data <- hc_data[hc_data$id %in% c("id_1", "id_5", "id_20"), ]
#' sel_data <- sel_data[order(sel_data$type, decreasing = TRUE), ]
#' plot_stiff(sel_data, idcol = "id", group = "type")
#'
#' # Stiff plot with group and custom fill
#' plot_stiff(
#'   hc_data,
#'   idcol = "id",
#'   group = "type",
#'   group_custom = TRUE,
#'   fill = c("lightblue", "orange"),
#'   color = c("red", "red")
#' )
#'
#' @import ggplot2
#' @import data.table
#' @importFrom grDevices colors
#' @importFrom scales breaks_pretty

plot_stiff <- function(data, 
                       base_unit = c("mg/L", "mmol/L", "meq/L"),
                       include_NO3 = NULL, # c("SO4", "Cl")
                       idcol = NULL,
                       group = NULL,
                       group_custom = FALSE,
                       nrow = NULL,
                       ncol = NULL,
                       scales = c("fixed", "free_x"),
                       facet_dir = c("v", "h"),
                       xlab = "meq/L", # NULL for no title
                       base_size = 12,
                       bold_label = FALSE,
                       color = "black",
                       fill = "grey40",
                       axis_title_size = 14,
                       label_border_color = "transparent", # possible to add color e.g. "black"
                       plot_title = NULL,
                       legend.position = "bottom",
                       legend.title = NULL,
                       show_label = TRUE,
                       bg_color = "#FFFFFF") {
  
  # Labels ------------------------------------------------------------------
  stiff_labels <- list(
    "Mg^'2+'",
    "Ca^'2+'",
    "Na^'+'~+~K^'+'",
    ifelse(!is.null(include_NO3) && include_NO3 == "SO4", "SO[4]^'2-'~+~NO[3]^'-'", "SO[4]^'2-'"),
    "HCO[3]^'-'~+~CO[3]^'2-'",
    ifelse(!is.null(include_NO3) && include_NO3 == "Cl", "Cl^'-'~+~NO[3]^'-'", "Cl^'-'")
  )
  
  if (bold_label) stiff_labels <- lapply(stiff_labels, \(x) paste0("bold(", x, ")"))
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  scales <- match.arg(scales)
  facet_dir <- match.arg(facet_dir)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  if (nrow(data) > 20) message("Number of Stiff diagrams is high; consider using save_plot() with a large width for better visualization.")
  
  # Define default ions used in Stiff diagram -------------------------------
  cations = c("Ca", "Mg", "Na", "K")
  anions = c("Cl", "SO4", "HCO3")
  
  # Add CO3 to anions if present in the dataset 
  if ("CO3" %in% names(data)) anions <- c(anions, "CO3")
  
  # Add NO3 to anions if include_NO3=TRUE & present in the dataset 
  if (!is.null(include_NO3)) {
    if (!(include_NO3 %in% c("SO4", "Cl"))) stop("`include_NO3` must be either 'SO4' or 'Cl' if not NULL.")
    if ("NO3" %in% names(data)) anions <- c(anions, "NO3")
    else if ("NO3_N" %in% names(data)) anions <- c(anions, "NO3_N")
    else stop("`include_NO3` is TRUE, but the 'NO3' or 'NO3_N' column is missing from the dataset.")
  }
  
  # All ions
  ions <- c(cations, anions)
  
  # Convert solutes mg/L to meq/L -------------------------------------------
  data <- data |> 
    convert_unit(ions = ions, base_unit = base_unit, to_unit = "meq/L", convert_to_species = TRUE)
  
  # Add K to Na
  data$Na_meq <- data$Na_meq + data$K_meq
  
  # Add NO3 to Cl if present
  if (!is.null(include_NO3)) {
    data[[paste0(include_NO3, "_meq")]] <- data[[paste0(include_NO3, "_meq")]] + data$NO3_meq
  }
  
  # Add CO3 to HCO3 if present
  if ("CO3_meq" %in% names(data)) {
    data$HCO3_meq <- data$HCO3_meq + data$CO3_meq
  }
  
  # Change value to negative for plotting
  data$Na_meq  <- -data$Na_meq
  data$Ca_meq  <- -data$Ca_meq
  data$Mg_meq  <- -data$Mg_meq
  
  # Handle group if null
  if (is.null(idcol)) {
    idcol <- "id"
    data[, (idcol) := factor(paste0("sample_", .I), levels = paste0("sample_", seq_len(.N)))]
  } else {
    data[[idcol]] <- factor(data[[idcol]], levels = unique(data[[idcol]]))
  }
  
  # Data to long
  stiff_data <- melt(
    data,
    id.vars = c(idcol, group),
    measure.vars = c("Na_meq", "Ca_meq", "Mg_meq", "SO4_meq", "HCO3_meq", "Cl_meq"),
    variable.name = "name",
    value.name = "value"
  )[, stiff_coords := fcase(
    name %in% c("Na_meq", "Cl_meq"), 6,
    name %in% c("Ca_meq", "HCO3_meq"), 4,
    name %in% c("Mg_meq", "SO4_meq"), 2,
    default = 99
  )]
  
  # Plot --------------------------------------------------------------------
  
  # Stiff limits
  if (scales == "fixed") {
    max_val <- max(abs(stiff_data$value), na.rm = TRUE)
    x_scale_limits <- c(-max_val, max_val)
  } else {
    # Compute max absolute x for each facet group
    stiff_data[, max_abs := max(abs(value), na.rm = TRUE), by = idcol]
    
    # Create dummy data with ±max_abs to center zero
    x_limits <- stiff_data[, .(x = c(-max_abs[1], max_abs[1])), by = idcol]
    
    # Remove scale_x_continuous limits
    x_scale_limits <- NULL
  }
  
  # Palette
  palette <- palette.colors()
  
  if (!is.null(group)) {
    all_groups <- unique(stiff_data[[group]])
    n_group <- length(all_groups)
    
    if (n_group > length(palette)) {
      all_colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
      if (n_group > length(all_colors)) stop(paste0("Too much entities in the group (n=", n_group, ")"), call. = FALSE)
      palette <- sample(all_colors, n_group)
    }
  }
  
  # Strip label
  if (!show_label) {
    strip.text = element_blank()
    strip.background = element_blank()
  } else {
    strip.text = element_text(size = axis_title_size)
    strip.background = element_rect(fill = "transparent", colour = label_border_color)
  }
  
  # Validate grouping and aesthetics
  if (group_custom && is.null(group)) {
    stop("`group_custom = TRUE` requires a non-NULL `group`. Set `group` to a column name in the data.", call. = FALSE)
  }
  
  if (group_custom) {
    check_color <- length(color) == n_group
    check_fill  <- length(fill)  == n_group
    if (!any(check_color, check_fill)) {
      stop("When 'group_custom' is TRUE, at least one of 'color', and 'fill' must have the same length as the number of groups (", n_group, ").",
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
  } else if (!is.null(group)) {
    dif_aes <- (color == "black" & fill == "grey40")
    if (any(lengths(list(fill, color)) > 1) | !dif_aes) {
      stop("When using grouping (`group` is not NULL) without `group_custom = TRUE`, aesthetics 'fill', and 'color' must each be of length 1. ",
           "To specify group-level aesthetics, set `group_custom = TRUE` and provide vectors of length equal to the number of groups (", n_group, ").",
           call. = FALSE)
    }
  }
  
  geom_polygon_layer <- function(data, group, group_custom, color, fill) {
    if (is.null(group)) {
      geom_polygon(data = data, aes(value, stiff_coords), fill = fill, color = color)
    } else if (!group_custom) {
      geom_polygon(data = data, aes(value, stiff_coords, fill = .data[[group]]), color = color)
    } else {
      geom_polygon(data = data, aes(value, stiff_coords, fill = .data[[group]], color = .data[[group]]))
    }
  }
  
  scale_list <- function(group, group_custom, legend.title, palette, color, fill) {
    if (is.null(group)) return(NULL)
    if (!group_custom) {
      return(scale_fill_manual(name = legend.title, values = palette, na.value = palette[n_group]))
    } else {
      return(list(
        scale_fill_manual(name = legend.title, values = fill, na.value = fill[n_group]),
        scale_color_manual(name = legend.title, values = color, na.value = color[n_group])
      ))
    }
  }
  
  ggplot() +
    facet_wrap(as.formula(paste("~", idcol)), 
               strip.position = "bottom",
               nrow = nrow,
               ncol = ncol,
               scales = scales,
               dir = facet_dir) +
    {if (scales == "free_x") geom_blank(data = x_limits, aes(x = x))} +
    coord_cartesian(ylim = c(1, 7)) +
    geom_vline(xintercept = 0) +
    geom_polygon_layer(stiff_data, group, group_custom, color, fill) +
    scale_list(group, group_custom, legend.title, palette, color, fill) +
    scale_x_continuous(position = "top", 
                       labels = abs, 
                       limits = x_scale_limits,
                       breaks = scales::breaks_pretty(n = 6)) +
    scale_y_continuous(
      breaks = c(2, 4, 6),
      labels = parse(text = c(stiff_labels[[1]], 
                              stiff_labels[[2]], 
                              stiff_labels[[3]])),
      sec.axis = dup_axis(
        labels = parse(text = c(stiff_labels[[4]], 
                                stiff_labels[[5]], 
                                stiff_labels[[6]]))
      )
    ) +
    xlab(xlab) +
    {if (!is.null(plot_title)) ggtitle(plot_title)} +
    theme_classic(base_size = base_size) +
    theme(
      legend.position = legend.position,
      legend.key = element_rect(fill = bg_color),
      legend.background = element_rect(fill = bg_color),
      strip.text = strip.text,
      strip.background = strip.background,
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.spacing.x = unit(1.5, "lines")
    ) 
}
