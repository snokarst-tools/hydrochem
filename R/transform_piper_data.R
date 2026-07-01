#' Transform Ion Analyses into Piper Diagram Coordinates
#'
#' Converts hydrochemical data to coordinates for a Piper diagram. The function handles conversion from mg/L or mmol/L to meq/L, normalization to percent, and coordinate calculation. It can also work with data already in meq/L or percent form.
#'
#' @param data A `data.frame` or `data.table` containing ion concentrations.
#' @param cations A character vector of cation names (default: `c("Ca", "Mg", "Na", "K")`).
#' @param anions A character vector of anion names (default: `c("Cl", "SO4", "HCO3", "CO3")`).
#' @param base_unit Unit of input data. One of `"mg/L"`, `"mmol/L"`, `"meq/L"`, or `"percent"`. Default is `"mg/L"`.
#' @param group Optional character string. Name of the column in `data` used to group and color points in the Piper diagram by observation. If `NULL`, all points are plotted with the same style. Default is `NULL`.
#' @param label Optional column name in `data` used to label points. Default is `NULL` (no labels).
#' 
#' @return A `data.frame` with three columns: `"observation"`, `"x"`, and `"y"` corresponding to Piper diagram coordinates.
#' @noRd
#'
#' @import data.table

transform_piper_data <- function(data,
                                 cations = c("Ca", "Mg", "Na", "K"),
                                 anions = c("Cl", "SO4", "HCO3", "CO3"), 
                                 base_unit = c("mg/L", "mmol/L", "meq/L", "percent"),
                                 group = NULL,
                                 label = NULL) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  ions <- c(cations, anions)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Prepare percent data ----------------------------------------------------
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
  
  data$total_cat <- rowSums(data[, paste0(cations, "_meq"), with = FALSE], na.rm = TRUE)
  data$total_an <- rowSums(data[, paste0(anions, "_meq"), with = FALSE], na.rm = TRUE)
  
  for (ion in ions) {
    col <- paste0(ion, "_meq")
    type <- if (ion %in% cations) "cat" else "an"
    data[[paste0(ion, "_meq_p")]] <- 100 * data[[col]] / data[[paste0("total_", type)]]
  }
  
  # Transform data ----------------------------------------------------------
  Mg <- data$Mg_meq_p
  Ca <- data$Ca_meq_p
  Cl <- data$Cl_meq_p
  SO4 <- data$SO4_meq_p
  
  # Add NO3 to Cl if present
  if ("NO3_meq_p" %in% names(data)) {
    Cl <- Cl + data$NO3_meq_p
  }
  
  if (is.null(group)) {
    group = rep(1:length(Mg), 3)
  } else {
    group = rep(data[[group]], 3)
  }
  
  if (is.null(label)) {
    label = rep(1:length(Mg), 3)
  } else {
    label = rep(data[[label]], 3)
  }
  
  y1 <- Mg * 0.86603
  x1 <- 100 * (1 - (Ca / 100) - (Mg / 200))
  y2 <- SO4 * 0.86603
  x2 <- 120 + (100 * Cl / 100 + 0.5 * 100 * SO4 / 100)
  
  new_point <- function(x1, x2, y1, y2, grad = 1.73206) {
    b1 <- y1 - (grad * x1)
    b2 <- y2 - ( - grad * x2)
    M <- matrix(c(grad, -grad, -1, -1), ncol = 2)
    intercepts <- as.matrix(c(b1, b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x = t_mat[1, 1], y = t_mat[2, 1])
  }
  
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind", np_list)
  data.frame(observation = group, x = c(x1, x2, npoints$x), y = c(y = y1, y2, npoints$y), label = label)
}
