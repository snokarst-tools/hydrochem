#' Plot Collins Diagrams from Hydrochemical Data
#'
#' Generates Collins diagrams from hydrochemical data. 
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' Optionally groups data points and allows manual customization of plotting aesthetics.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param include_NO3 Logical. If `TRUE`, nitrate (`NO3`) is included. The `NO3` column must be present in the input data. Default is `FALSE`.
#' @param ratio Numeric. Aspect ratio between x and y axes in the plot. Default is `0.05`.
#' @param idcol Character or `NULL`. Column name used to identify and facet individual diagrams. If `NULL`, synthetic sample IDs are generated. Default is `NULL`.
#' @param nrow,ncol Integers. Number of rows and columns for the facet layout. Default is `NULL` (automatic layout).
#' @param facet_dir Character. Facet layout direction. Either `"v"` for vertical or `"h"` for horizontal. Default is `"v"`.
#' @param ylab Character. Label for the y-axis. Set `NULL` for no axis. Default is `"%meq/L"`.
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
#' @return A `ggplot` object representing the Collins diagram(s).
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' hc_data <- hc_data[1:20, ]
#' plot_collins(hc_data, idcol = "id")
#'
#' @import ggplot2
#' @import data.table

plot_collins <- function(data, 
                         base_unit = c("mg/L", "mmol/L", "meq/L"),
                         include_NO3 = FALSE, 
                         ratio = 0.05,
                         idcol = NULL,
                         nrow = NULL,
                         ncol = NULL,
                         facet_dir = c("v", "h"),
                         ylab = "%meq/L", # NULL for no axis
                         base_size = 12,
                         border_color = "transparent",
                         border_width = 0.5,
                         axis_title_size = 14,
                         label_border_color = "transparent", # possible to add color e.g. "black"
                         plot_title = NULL,
                         legend.position = "right",
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
  
  collins_labels <- c("K_meq_p" = "K^'+'", 
                      "Na_meq_p" = "Na^'+'",
                      "Mg_meq_p" = "Mg^'2+'", 
                      "Ca_meq_p" = "Ca^'2+'",
                      "NO3_meq_p" = "NO3^'-'",
                      "Cl_meq_p" = "Cl^'-'", 
                      "SO4_meq_p" = "SO4^'2-'",
                      "HCO3_meq_p" = "HCO3^'-'")
  palette <- palette.colors()
  names(palette) <- meq_p_names
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  facet_dir <- match.arg(facet_dir)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  if (nrow(data) > 20) message("Number of Collins diagrams is high; consider using save_plot() with a large width for better visualization.")
  
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
  collins_data <- melt(
    data,
    id.vars = c(idcol),
    measure.vars = paste0(ions, "_meq_p"),
    variable.name = "name",
    value.name = "value"
  )
  
  # Order of colors
  collins_data$name <- factor(collins_data$name, levels = c("K_meq_p", "Na_meq_p", "Mg_meq_p", "Ca_meq_p",
                                                            "NO3_meq_p", "Cl_meq_p", "SO4_meq_p", "HCO3_meq_p"))
  
  if (!include_NO3) {
    palette <- palette[names(palette) != "NO3_meq_p"]
    collins_labels <- collins_labels[names(collins_labels) != "NO3_meq_p"]
  }
  
  collins_data$ions_type_cat_an <- ions_meta$type[match(gsub("_meq_p", "", collins_data$name), ions_meta$ion_r)]
  
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
               strip.position = "top",
               nrow = nrow,
               ncol = ncol,
               dir = facet_dir) +
    coord_fixed(ratio = ratio) +
    geom_bar(data = collins_data, aes(factor(ions_type_cat_an, c("cation", "anion")), value, fill = name), width = 1, stat = "identity",
             color = border_color, linewidth = border_width) +
    scale_fill_manual(values = palette, labels = parse(text = collins_labels), breaks = names(collins_labels)) +
    {if (!is.null(plot_title)) ggtitle(plot_title)} +
    scale_x_discrete(expand = expansion(mult = c(1, 1))) +
    scale_y_continuous() +
    ylab(ylab) +
    theme_void(base_size = base_size) +
    theme(
      legend.position = legend.position,
      legend.key = element_rect(fill = bg_color),
      legend.title = element_blank(),
      legend.background = element_rect(fill = bg_color, color = "transparent"),
      strip.text = strip.text,
      strip.background = strip.background,
      axis.line.y = element_line(),
      axis.title.y = element_text(),
      axis.text.y = element_text(margin = margin(r = 5)),
      axis.ticks.y = element_line(),
      axis.ticks.length.y = unit(5, "pt"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      plot.background = element_rect(fill = bg_color, color = bg_color)
    ) +
    guides(fill = guide_legend(ncol = 2))
  
  if (is.null(ylab)) {
    x <- x + 
      theme(
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  
  x
}
