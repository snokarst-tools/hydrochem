#' Plot Schoeller Diagrams from Hydrochemical Data
#'
#' Generates Schoeller diagrams from hydrochemical data.  
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param include_NO3 Logical. If `TRUE`, nitrate (`NO3`) is included. The `NO3` column must be present in the input data. Default is `FALSE`.
#' @param mg_guide Character. Display mode for mg/L horizontal guide lines in the Schoeller diagram. `"full"` shows lines with labels, `"simple"` shows only dashed lines, and `"none"` hides them completely. Default is `"full"`.
#' Major ticks (0.01, 0.1, 1, 10) are emphasized. Labels appear to the left for `Na` and `HCO3`, to the right for `K` and `CO3` (if present).
#' @param group Optional character string. Name of the column used for coloring groups. Default is `NULL`.
#' @param group_custom Logical. If `TRUE`, enables manual aesthetics for each group defined in `group`. Requires a vector `color` of the same length as number of groups. Default is `FALSE`.
#' @param base_size Numeric. Base font size for the plot. Default is `12`.
#' @param bold_label Logical. If `TRUE`, displays axis labels in bold. Default is `FALSE`.
#' @param line_color Character or vector. Line color(s). If `group_custom = TRUE`, must be a vector. Default is `"black"`.
#' @param line_width Numeric. Line width. Default is `1`.
#' @param plot_title Optional character string. Title of the plot. Default is NULL (no title displayed).
#' @param legend.position Position of the legend. Accepts `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`, or a numeric vector of length 2 for relative coordinates, e.g., `c(1, 0.5)`. Default is `"bottom"`.
#' @param legend.title A character string for the title of the legend. If `NULL`, no legend title is displayed. Default is `NULL`.
#' @param bg_color Character. Background color of the plot area and panel. Accepts valid color names or hex codes (e.g., "red", "#FFFFFF"). Default is "#FFFFFF".
#'
#' @return A `ggplot` object representing the Schoeller diagram.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' hc_data <- hc_data[1:20, ] # keep first 20 rows
#' 
#' # Basic Schoeller plot
#' plot_schoeller(hc_data)
#' 
#' # Remove legend
#' plot_schoeller(hc_data, legend.position = "none")
#'
#' # Schoeller plot with mg/L guides hidden
#' plot_schoeller(hc_data, mg_guide = "none")
#'
#' # Specify a group by unique id
#' plot_schoeller(hc_data, group = "id")
#'
#' # Schoeller plot with group coloring
#' plot_schoeller(hc_data, group = "type")
#'
#' # Schoeller plot with custom group colors
#' plot_schoeller(
#'   hc_data,
#'   group = "type",
#'   group_custom = TRUE,
#'   line_color = c("red", "blue")
#' )
#'
#' # Schoeller plot with NO3 included
#' plot_schoeller(hc_data, include_NO3 = TRUE)
#'
#' @import ggplot2
#' @import data.table

plot_schoeller <- function(data, 
                           base_unit = c("mg/L", "mmol/L", "meq/L"),
                           include_NO3 = FALSE,
                           mg_guide = c("full", "simple", "none"),
                           group = NULL,
                           group_custom = FALSE,
                           base_size = 12,
                           bold_label = FALSE,
                           line_color = "black",
                           line_width = 1,
                           plot_title = NULL,
                           legend.position = "bottom",
                           legend.title = NULL,
                           bg_color = "#FFFFFF") {
  
  # Labels ------------------------------------------------------------------
  schoeller_labels <- c(
    "Ca^'2+'",
    "Mg^'2+'",
    bquote(phantom("l")^phantom("1")~Na^"+"~+~K^"+"~phantom("l")^phantom("1")),
    bquote(phantom("l")^phantom("1")~Cl^"-"~phantom("l")^phantom("1")),
    "SO[4]^'2-'",
    "HCO[3]^'-'~+~CO[3]^'2-'",
    if (include_NO3) bquote(phantom("l")^phantom("1")~NO[3]^"-"~phantom("l")^phantom("1")) else NULL
  )
  
  if (bold_label) {
    schoeller_labels <- c(
      "bold(Ca^'2+')",
      "bold(Mg^'2+')",
      "phantom('l')^phantom('1')~bold(Na^'+')~+~bold(K^'+')~phantom('l')^phantom('1')",
      "phantom('l')^phantom('1')~bold(Cl^'-')~phantom('l')^phantom('1')",
      "bold(SO[4]^'2-')",
      "bold(HCO[3]^'-')~+~bold(CO[3]^'2-')",
      if (include_NO3) "phantom('l')^phantom('1')~bold(NO[3]^'-')~phantom('l')^phantom('1')" else NULL
    )
  }
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  mg_guide <- match.arg(mg_guide)
  measure.vars <- c("Ca_meq", "Mg_meq", "Na_meq", "Cl_meq", "SO4_meq", "HCO3_meq")
  log_range <- c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)
  log_range_label <- c(0, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Define default ions used in Schoeller diagram -------------------------------
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
  
  # All ions
  ions <- c(cations, anions)
  
  # Convert solutes mg/L to meq/L -------------------------------------------
  data <- data |> 
    convert_unit(ions = ions, base_unit = base_unit, to_unit = "meq/L", convert_to_species = TRUE)

  # Replace with species name
  match_idx <- match(ions, ions_meta$ion_r)
  replacements <- ions_meta$species_names[match_idx]
  ions <- ifelse(!is.na(replacements), replacements, ions)
  
  # Add K to Na
  data$Na_meq <- data$Na_meq + data$K_meq
  
  # Add NO3 to Cl if present
  if (include_NO3) {
    measure.vars <- c(measure.vars, "NO3_meq")
  }
  
  # Add CO3 to HCO3 if present
  if ("CO3_meq" %in% names(data)) {
    data$HCO3_meq <- data$HCO3_meq + data$CO3_meq
  }
  
  # Handle group if null
  idcol <- "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  data[, (idcol) := factor(paste0("sample_", .I), levels = paste0("sample_", seq_len(.N)))]
  
  if (is.null(group)) {
    group <- "group"
    data[, (group) := factor(paste0("sample_", .I), levels = paste0("sample_", seq_len(.N)))]
  } else {
    data[[group]] <- factor(data[[group]], levels = unique(data[[group]]))
  }
  
  # Data to long
  schoeller_data <- melt(
    data,
    id.vars = c(idcol, group),
    measure.vars = measure.vars,
    variable.name = "name",
    value.name = "value"
  )
  
  # Add numeric x for continuous plotting
  schoeller_data[, x := as.numeric(factor(name, levels = unique(name)))]
  
  # Check if 0 values
  zero_index <- schoeller_data$value == 0
  zero_name <- unique(schoeller_data$name[zero_index])
  if (length(zero_name) > 0) {
    message(paste0(gsub("_meq", "", zero_name), " have values equal to 0 that have been converted to 0.0001 for lisibility on log scale."))
    schoeller_data$value[zero_index] <- 0.0001
  }
  
  # Plot --------------------------------------------------------------------
  range_vals <- range(schoeller_data$value, na.rm = TRUE)
  
  # Palette
  palette <- palette.colors()
  
  if (!is.null(group)) {
    all_groups <- unique(schoeller_data[[group]])
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
    check_color <- length(line_color) == n_group
    if (!any(check_color)) {
      stop("When 'group_custom' is TRUE, 'line_color' must have the same length as the number of groups (", n_group, ").",
           call. = FALSE)
    }
  } else if (!is.null(group)) {
    dif_aes <- (line_color == "black")
    if (any(lengths(list(line_color)) > 1) | !dif_aes) {
      stop("When using grouping (`group` is not NULL) without `group_custom = TRUE`, aesthetic 'line_color' must be of length 1. ",
           "To specify group-level aesthetics, set `group_custom = TRUE` and provide vector of length equal to the number of groups (", n_group, ").",
           call. = FALSE)
    }
  }
  
  # Complex version with mg_guide == "full"
  if (mg_guide == "full") {
    log_ticks <- as.numeric(outer(1:9, setdiff(log_range, c(0.0001, 0.001)))) # remove very low values
    
    mg_ions <- ions
    if (!("CO3" %in% ions)) mg_ions <- c(mg_ions, "CO3")
    
    log_guides <- as.data.frame(setNames(rep(list(log_ticks), length(mg_ions)), mg_ions)) |> 
      convert_unit(ions = mg_ions, base_unit = "mg/L", to_unit = "meq/L")
    
    log_guides_long <- do.call(rbind, lapply(seq_along(mg_ions), function(i) {
      ion <- mg_ions[i]
      x_pos <- c(Ca = 1, Mg = 2, Na = 3, K = 3, Cl = 4, SO4 = 5, HCO3 = 6, CO3 = 6, NO3 = 7)[[ion]]
      data.frame(
        ion   = ion,
        x     = x_pos,
        y     = log_guides[[paste0(ion, "_meq")]],
        label = log_guides[[ion]]
      )
    }))
    
    log_guides_long$label[!log_guides_long$label %in% log_range] <- NA_real_
    
    log_guides_long$xstart <- with(log_guides_long,
                                   ifelse(label %in% log_range & ion %in% c("Na", "HCO3"), x - 0.05,
                                          ifelse(label %in% log_range & ion %in% c("K", "CO3"), x,
                                                 ifelse(label %in% log_range, x - 0.05,
                                                        ifelse(ion %in% c("Na", "HCO3"), x - 0.03,
                                                               ifelse(ion %in% c("K", "CO3"), x, x - 0.03))))) 
    )
    
    log_guides_long$xend <- with(log_guides_long,
                                 ifelse(label %in% log_range & ion %in% c("Na", "HCO3"), x,
                                        ifelse(label %in% log_range & ion %in% c("K", "CO3"), x + 0.05,
                                               ifelse(label %in% log_range, x + 0.05,
                                                      ifelse(ion %in% c("Na", "HCO3"), x,
                                                             ifelse(ion %in% c("K", "CO3"), x + 0.02, x + 0.03))))) 
    )
    
    geom_schoeller_labels <- function() {
      data_right <- subset(log_guides_long, !ion %in% c("Na", "HCO3") & !is.na(label))
      data_left  <- subset(log_guides_long,  ion %in% c("Na", "HCO3") & !is.na(label))
      
      list(
        geom_segment(
          data = log_guides_long,
          aes(x = xstart, xend = xend, y = y, yend = y),
          color = "grey50", linewidth = 0.3
        ),
        geom_text(
          data = data_right,
          aes(x = x + 0.1, y = y, label = label),
          size = 2.5, hjust = 0
        ),
        geom_text(
          data = data_left,
          aes(x = x - 0.1, y = y, label = label),
          size = 2.5, hjust = 1
        )
      )
    }
  }
  
  # Plot
  geom_line_layer <- function(data, idcol, group, group_custom, line_color, line_width) {
    if (is.null(group)) {
      geom_line(data = data, aes(x, value, group = .data[[idcol]]), color = line_color, linewidth = line_width)
    } else {
      geom_line(data = data, aes(x, value, group = .data[[idcol]], color = .data[[group]]), linewidth = line_width)
    }
  }
  
  scale_list <- function(group, group_custom, legend.title, palette, line_color) {
    if (is.null(group)) return(NULL)
    if (!group_custom) {
      return(scale_color_manual(name = legend.title, values = palette, na.value = palette[n_group]))
    } else {
      return(list(
        scale_color_manual(name = legend.title, values = line_color, na.value = line_color[n_group])
      ))
    }
  }
  
  ggplot() +
    {if (mg_guide != "none") geom_vline(xintercept = unique(schoeller_data$x), 
                                        linetype = ifelse(mg_guide == "full", "solid", "dashed"), 
                                        alpha = 0.5)} +
    {if (mg_guide == "full") geom_schoeller_labels()} +
    geom_line_layer(schoeller_data, idcol, group, group_custom, line_color, line_width) +
    scale_list(group, group_custom, legend.title, palette, line_color) +
    scale_x_continuous(breaks = unique(schoeller_data$x),
                       labels = parse(text = schoeller_labels)) +
    scale_y_continuous(
      trans = "log10",
      breaks = log_range,
      labels = log_range_label,
      sec.axis = dup_axis()
    ) +
    ggplot2::annotation_logticks(sides = "rl") +
    coord_cartesian(ylim = c(range_vals[1], range_vals[2])) +
    ylab("meq/L") +
    {if (!is.null(plot_title)) ggtitle(plot_title)} +
    theme_classic(base_size = base_size) +
    theme(
      legend.position = legend.position,
      legend.key = element_rect(fill = bg_color),
      legend.background = element_rect(fill = bg_color),
      axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.spacing.x = unit(1.5, "lines")
    ) 
}
