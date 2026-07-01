#' Calculate Total Dissolved Solids (TDS)
#'
#' Computes the total dissolved solids (TDS) of a hydrochemical dataset by summing the concentrations
#' of selected ions in mg/L. Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"mg/L"` prior to calculation.
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param method Character string indicating the calculation method. One of:
#'   - `"major"` (default): uses major ions (Ca, Mg, Na, K, HCO3, CO3, Cl, SO4, NO3),
#'   - `"major_si"`: adds SiO2 to the major ions (can be calculated with Si if only available), 
#'   - `"all_ions"`: uses all available ions in the dataset and SiO2.
#'
#' @return A numeric vector of TDS values, one per row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' tds(hc_data, base_unit = "mg/L")
#'
#' @import data.table

tds <- function(data,
                base_unit = c("mg/L", "mmol/L", "meq/L"),
                method = c("major", "major_si", "all_ions")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  method <- match.arg(method)
  keep <- switch (method,
                  "major" = "major",
                  "major_si" = c("major", "major_si"),
                  "all_ions" = c("major", "major_si", "all_ions")
  )
  
  # Get ions ----------------------------------------------------------------
  valid_ions <- ions_meta$ion_r[ions_meta$TDS %in% keep]
  ions <- names(data)[names(data) %in% valid_ions]
  
  # Check for missing ions
  missing <- ions_meta$ion_r[ions_meta$TDS %in% keep & !(ions_meta$ion_r %in% ions)]
  if (length(missing) > 0) {
    needed <- c("Ca", "Mg", "Na", "K", "HCO3", "Cl", "SO4")
    missing_major <- needed[needed %in% missing]
    if (length(missing_major) > 0) stop("Missing obligatory ions: ", paste(missing_major, collapse = ", "), call. = FALSE)
    if ("CO3" %in% missing) warning("CO3 is missing", call. = FALSE)
    if (all(c("Si", "SiO2") %in% missing)) warning("Si/SiO2 is missing", call. = FALSE)
  }
  
  # Convert to mg/L --------------------------------------------------------
  dt <- convert_unit(
    data,
    ions = ions,
    base_unit = base_unit,
    to_unit = "mg/L",
    convert_to_species = TRUE
  )
  
  # Calculate with Si or SiO2 depending on dataset
  if (method %in% c("major_si", "all_ions")) {
    # Case if both Si/SiO2 are present -- only keep SiO2
    if (all(c("Si", "SiO2") %in% ions)) {
      ions <- setdiff(ions, "Si")
    # Case if only Si is present -- convert to SiO2 = Si * 2.13922
    } else if ("Si" %in% ions) {
      dt$Si_mg <- dt$Si_mg * (ions_meta$masse_molaire[match("SiO2", ions_meta$ion_r)] /
                          ions_meta$masse_molaire[match("Si", ions_meta$ion_r)])
    }
  }
  
  # Replace with species name
  match_idx <- match(ions, ions_meta$ion_r)
  replacements <- ions_meta$species_names[match_idx]
  ions <- ifelse(!is.na(replacements), replacements, ions)
  
  # TDS ---------------------------------------------------------------------
  message(paste0("TDS computed with: ", paste(ions, collapse = ", ")))
  tds <- rowSums(dt[, paste0(ions, "_mg"), with = FALSE], na.rm = TRUE)
  return(tds)
}
