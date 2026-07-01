#' Plot Stabler Diagrams from Hydrochemical Data
#'
#' Generates Stabler diagrams from hydrochemical data.
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param include_NO3 Logical. If `TRUE`, nitrate (`NO3`) is included. The `NO3` column must be present in the input data. Default is `FALSE`.
#' @param ratio Numeric. Aspect ratio between x and y axes in the plot. Default is `10`.
#' @param idcol Character or `NULL`. Column name used to identify and facet individual diagrams. If `NULL`, synthetic sample IDs are generated. Default is `NULL`.
#' @param nrow,ncol Integers. Number of rows and columns for the facet layout. Default is `NULL` (automatic layout).
#' @param facet_dir Character. Facet layout direction. Either `"v"` for vertical or `"h"` for horizontal. Default is `"v"`.
#' @param xlab Character. Label for the x-axis. Set `NULL` for no axis Default is `"%meq/L"`.
#' @param base_size Numeric. Base font size for plot text. Default is `12`.
#' @param border_color Color of the bar border. Default is `"transparent"`.
#' @param border_width Width of the bar border. Default is `0.5`.
#' @param axis_title_size Numeric. Font size for strip labels. Default is `14`.
#' @param label_border_color Character. Border color for facet strip labels. Use `"transparent"` for no border. Default is `"transparent"`.
#' @param plot_title Optional character string. Title of the plot. Default is NULL (no title displayed).
#' @param legend.position Position of the legend. Accepts `"top"`, `"bottom"`, `"left"`, `"right"`, `"none"`, or a numeric vector of length 2 for relative coordinates, e.g., `c(1, 0.5)`. Default is `"bottom"`.
#' @param legend.title A character string for the title of the legend. If `NULL`, no legend title is displayed. Default is `NULL`.
#' @param show_label Logical. If `TRUE`, displays strip labels. Default is `TRUE`.
#' @param bg_color Character. Background color of the plot area and panel. Accepts valid color names or hex codes (e.g., "red", "#FFFFFF"). Default is "#FFFFFF".
#'
#' @return A `ggplot` object representing the Stabler diagram(s).
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' hc_data <- hc_data[1:20, ]
#' plot_stabler(hc_data, idcol = "id")
#'
#' @import ggplot2
#' @import data.table

plot_stabler <- function(data, 
                         base_unit = c("mg/L", "mmol/L", "meq/L"),
                         include_NO3 = FALSE, 
                         ratio = 10,
                         idcol = NULL,
                         nrow = NULL,
                         ncol = NULL,
                         facet_dir = c("v", "h"),
                         xlab = "%meq/L", # NULL for no axis
                         base_size = 12,
                         border_color = "transparent",
                         border_width = 0.5,
                         axis_title_size = 14,
                         label_border_color = "transparent", # possible to add color e.g. "black"
                         plot_title = NULL,
                         legend.position = "bottom",
                         legend.title = NULL,
                         show_label = TRUE,
                         bg_color = "#FFFFFF") {
  
  # Labels ------------------------------------------------------------------
  meq_p_names <- c("K_meq_p", 
                   "Na_meq_p", 
                   "Mg_meq_p", 
                   "Ca_meq_p", 
                   "Cl_meq_p",
                   "SO4_meq_p", 
                   "HCO3_meq_p",
                   "NO3_meq_p")
  
  stabler_labels <- c("Ca_meq_p" = "Ca^'2+'", 
                      "HCO3_meq_p" = "HCO3^'-'",
                      "Mg_meq_p" = "Mg^'2+'", 
                      "SO4_meq_p" = "SO4^'2-'",
                      "Na_meq_p" = "Na^'+'",
                      "Cl_meq_p" = "Cl^'-'", 
                      "K_meq_p" = "K^'+'", 
                      "NO3_meq_p" = "NO3^'-'")
  palette <- palette.colors()
  names(palette) <- meq_p_names
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  facet_dir <- match.arg(facet_dir)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  if (nrow(data) > 20) message("Number of Stabler diagrams is high; consider using save_plot() with a large width for better visualization.")
  
  # Define default ions used in Stiff diagram -------------------------------
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
  match_idx <- match(cations, ions_meta$ion_r)
  replacements <- ions_meta$species_names[match_idx]
  cations <- ifelse(!is.na(replacements), replacements, cations)
  
  match_idx <- match(anions, ions_meta$ion_r)
  replacements <- ions_meta$species_names[match_idx]
  anions <- ifelse(!is.na(replacements), replacements, anions)
  
  ions <- c(cations, anions)
  
  # Add CO3 to HCO3 if present
  if ("CO3_meq" %in% names(data)) {
    data$HCO3_meq <- data$HCO3_meq + data$CO3_meq
    anions <- setdiff(anions, "CO3")
    ions <- setdiff(ions, "CO3")
  }
  
  # Handle group if null
  if (is.null(idcol)) {
    idcol <- "id"
    data[, (idcol) := factor(paste0("sample_", .I), levels = paste0("sample_", seq_len(.N)))]
  } else {
    data[[idcol]] <- factor(data[[idcol]], levels = unique(data[[idcol]]))
  }
  
  # Prepare percent data ----------------------------------------------------
  data$total_cat <- rowSums(data[, paste0(cations, "_meq"), with = FALSE], na.rm = TRUE)
  data$total_an <- rowSums(data[, paste0(anions, "_meq"), with = FALSE], na.rm = TRUE)
  
  for (ion in ions) {
    col <- paste0(ion, "_meq")
    type <- if (ion %in% cations) "cat" else "an"
    data[[paste0(ion, "_meq_p")]] <- 100 * data[[col]] / data[[paste0("total_", type)]]
  }
  
  # Prepare data ------------------------------------------------------------
  
  # Data to long
  stabler_data <- melt(
    data,
    id.vars = c(idcol),
    measure.vars = paste0(ions, "_meq_p"),
    variable.name = "name",
    value.name = "value"
  )
  
  # Order of colors
  stabler_data$name <- factor(stabler_data$name, levels = c("K_meq_p", "Na_meq_p", "Mg_meq_p", "Ca_meq_p",
                                                            "NO3_meq_p", "Cl_meq_p", "SO4_meq_p", "HCO3_meq_p"))

  if (!include_NO3) {
    palette <- palette[names(palette) != "NO3_meq_p"]
    stabler_labels <- stabler_labels[names(stabler_labels) != "NO3_meq_p"]
  }
  
  stabler_data$ions_type_cat_an <- ions_meta$type[match(gsub("_meq_p", "", stabler_data$name), ions_meta$ion_r)]
  
  # Plot --------------------------------------------------------------------
 
  # Strip label
  if (!show_label) {
    strip.text = element_blank()
    strip.background = element_blank()
  } else {
    strip.text = element_text(size = axis_title_size)
    strip.background = element_rect(fill = "transparent", colour = label_border_color)
  }
  
  x <- ggplot() +
    facet_wrap(as.formula(paste("~", idcol)), 
               strip.position = "left",
               nrow = nrow,
               ncol = ncol,
               dir = facet_dir) +
    coord_fixed(ratio = ratio) +
    geom_bar(data = stabler_data, aes(value, ions_type_cat_an, fill = name), width = 1, stat = "identity",
             color = border_color, linewidth = border_width) +
    scale_fill_manual(values = palette, labels = parse(text = stabler_labels), breaks = names(stabler_labels)) +
    {if (!is.null(plot_title)) ggtitle(plot_title)} +
    scale_x_continuous(position = "top") +
    scale_y_discrete(expand = expansion(mult = c(1, 1))) +
    xlab(xlab) +
    theme_void(base_size = base_size) +
    theme(
      legend.position = legend.position,
      legend.key = element_rect(fill = bg_color),
      legend.title = element_blank(),
      legend.background = element_rect(fill = bg_color, color = "transparent"),
      strip.text = strip.text,
      strip.background = strip.background,
      axis.line.x.top = element_line(),
      axis.title.x.top = element_text(),
      axis.text.x.top = element_text(margin = margin(b = 2)),
      axis.ticks.x.top = element_line(),
      axis.ticks.length.x = unit(5, "pt"),
      axis.line.x = element_line(),
      axis.title.x = element_text(),
      axis.text.x = element_text(margin = margin(b = 2)),
      axis.ticks.x = element_line(),
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      plot.background = element_rect(fill = bg_color, color = bg_color)
    )
  
  if (is.null(xlab)) {
    x <- x + 
      theme(
        axis.line.x.top = element_blank(),
        axis.title.x.top = element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
      )
  }
  
  x
}
