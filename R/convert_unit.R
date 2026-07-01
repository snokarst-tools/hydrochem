#' Convert Ion Concentrations Between Units (mg/L, mmol/L, meq/L)
#'
#' Converts concentrations of selected ions (cations and anions) between supported units:
#' `"mg/L"`, `"mmol/L"`, and `"meq/L"`. Input columns must be named using
#' plain ion names (e.g., `"Ca"`, `"Cl"`). Output columns are added with suffixes:
#' `_mg`, `_mmol`, and `_meq` depending on the `to_unit` function parameter.
#'
#' @param data A `data.frame` or `data.table` with numeric columns named after ions (e.g., `"Ca"`, `"Cl"`).
#' @param ions A character vector of ions names. Default is `c("Ca", "Mg", "Na", "K", "Cl", "SO4", "HCO3")`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param to_unit Unit of the output unit. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param convert_to_species Logical. If `TRUE`, converts known elemental forms (e.g., `"NO3_N"`, `"PO4_P"`, `"H2S_HS_S"`) to their compound species (`"NO3"`, `"PO4"`, `"H2S"`).
#'
#' @return A `data.table` with original columns and new columns suffixed based on the `to_unit` function parameter.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' convert_unit(hc_data, base_unit = "mg/L", to_unit = "meq/L")
#'
#' @import data.table

convert_unit <- function(data,
                         ions = c("Ca", "Mg", "Na", "K", "Cl", "SO4", "HCO3"),
                         base_unit = c("mg/L", "mmol/L", "meq/L"),
                         to_unit = c("mg/L", "mmol/L", "meq/L"),
                         convert_to_species = FALSE) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  to_unit <- match.arg(to_unit)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Atomic weights (mg/mmol)
  weights <- setNames(ions_meta$masse_molaire, ions_meta$ion_r)
  
  # Charges (meq/mmol)
  charges <- setNames(ions_meta$charge, ions_meta$ion_r)
  
  # Species convert and name for NO3_N, NO2_N, PO4_P, H2S_HS_S
  species_convert <- setNames(ions_meta$species_convert, ions_meta$ion_r)
  species_name <- setNames(ions_meta$species_name, ions_meta$ion_r)
  
  # Convert from base_unit to meq/L
  for (ion in ions) {
    if (!ion %in% names(data)) stop(paste0("Ion missing: ", ion))
    
    val <- data[[ion]]
    meq_val <- switch(base_unit,
                      "mg/L" = val * charges[[ion]] / weights[[ion]],
                      "mmol/L" = val * charges[[ion]],
                      "meq/L" = val
    )
    
    data[[paste0(ion, "_meq")]] <- meq_val
  }
  
  # Convert meq/L to target unit
  for (ion in ions) {
    meq_col <- paste0(ion, "_meq")
    if (!meq_col %in% names(data)) next
    
    out <- switch(to_unit,
                  "meq/L" = data[[meq_col]],
                  "mmol/L" = data[[meq_col]] / charges[[ion]],
                  "mg/L" = data[[meq_col]] * weights[[ion]] / charges[[ion]]
    )
    
    suffix <- switch(to_unit,
                     "meq/L" = "_meq",
                     "mmol/L" = "_mmol",
                     "mg/L" = "_mg"
    )
    
    if (convert_to_species) {
      if (ion %in% ions_meta$ion_r[which(!is.na(ions_meta$species_names))]) {
        # Condition to get the right amount of NO3, NO2, etc. from NO3_N, NO2_N, .. if mg/L
        if (to_unit == "mg/L") {
          out <- out * species_convert[[ion]]
        }
        
        # Convert to species form
        message(sprintf("Ion '%s' converted to species form '%s'", ion, species_name[[ion]]))
        ion <- species_name[[ion]]
      }
    }
    
    data[[paste0(ion, suffix)]] <- out
  }
  
  if (to_unit != "meq/L") {
    data <- data[, setdiff(names(data), paste0(ions, "_meq")), with = FALSE]
  }
  
  return(data)
}
