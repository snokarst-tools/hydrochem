#' Transform Ion Analyses into ternary Diagram Coordinates
#'
#' Converts hydrochemical data to coordinates for a ternary diagram. The function handles conversion from mg/L or mmol/L to meq/L, normalization to percent, and coordinate calculation. It can also work with data already in meq/L or percent form.
#'
#' @param data A `data.frame` or `data.table` containing ion concentrations.
#' @param first,second,third Character strings. Names of the three solute columns to be plotted on the ternary diagram.
#' @param base_unit Unit of input data. One of `"mg/L"`, `"mmol/L"`, `"meq/L"`. Default is `"mg/L"`.
#' @param to_unit Unit of the output ion concentrations. One of `"meq/L"`, `"mmol/L"`, or `"mg/L"`. Default is `"meq/L"`.
#' @param group Optional character string. Name of the column in `data` used to group and color points in the ternary diagram by observation. If `NULL`, all points are plotted with the same style. Default is `NULL`.
#' @param label Optional column name in `data` used to label points. Default is `NULL` (no labels).
#'
#' @return A `data.frame` with three columns: `"observation"`, `"x"`, and `"y"` corresponding to ternary diagram coordinates.
#' @noRd
#'
#' @import data.table

transform_ternary_data <- function(data,
                                   first,
                                   second,
                                   third,
                                   base_unit = c("mg/L", "mmol/L", "meq/L"),
                                   to_unit = c("meq/L", "mmol/L", "mg/L"),
                                   group = NULL,
                                   label = NULL) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  to_unit <- match.arg(to_unit)
  ions <- c(first, second, third)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Prepare percent data ----------------------------------------------------
  data <- data |> 
    convert_unit(ions = ions, base_unit = base_unit, to_unit = to_unit)
  
  current_unit <- sub("/L", "", to_unit)
  
  data$total <- rowSums(data[, paste0(ions, "_", current_unit), with = FALSE], na.rm = TRUE)
  
  for (ion in ions) {
    col <- paste0(ion, "_", current_unit)
    data[[paste0(ion, "_", current_unit, "_p")]] <- 100 * data[[col]] / data$total
  }
  
  # Transform data ----------------------------------------------------------
  first_col <- data[[paste0(first, "_", current_unit, "_p")]]
  second_col <- data[[paste0(second, "_", current_unit, "_p")]]
  
  if (is.null(group)) {
    group = rep(1:length(first_col), 1)
  } else {
    group = rep(data[[group]], 1)
  }
  
  if (is.null(label)) {
    label = rep(1:length(first_col), 3)
  } else {
    label = rep(data[[label]], 3)
  }
  
  x1 <- 100 * (1 - (first_col / 100) - (second_col / 200))
  y1 <- second_col * 0.86603
  
  data.frame(observation = group, x = x1, y = y1, label = label)
}
