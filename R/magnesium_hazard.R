#' Calculate Magnesium Hazard (MH)
#'
#' Computes the Magnesium Hazard (MH) index from hydrochemical data using
#' calcium and magnesium concentrations. Input concentrations may be provided in
#' `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted to `"meq/L"`
#' prior to calculation.
#'
#' The function checks that both required ions (`Ca`, `Mg`) are present in the
#' input dataset and throws an error if either is missing.
#'
#' @param data A `data.frame` or `data.table` containing the required ion
#'   columns `"Ca"` and `"Mg"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#'
#' @return A numeric vector of Magnesium Hazard values (in %), one per row of
#'   `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' magnesium_hazard(hc_data, base_unit = "mg/L")
#'
#' @import data.table

magnesium_hazard <- function(data,
                               base_unit = c("mg/L", "mmol/L", "meq/L")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  
  # Get ions ----------------------------------------------------------------
  ions <- c("Ca", "Mg")
  needed <- c("Ca", "Mg")
  present <- names(data)[names(data) %in% needed]
  
  # Check for missing ions
  missing <- needed[!(needed %in% present)]
  if (length(missing) > 0) {
    stop("Missing obligatory ions: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  # Convert to mg/L --------------------------------------------------------
  dt <- convert_unit(
    data,
    ions = ions,
    base_unit = base_unit,
    to_unit = "meq/L",
    convert_to_species = TRUE
  )
  
  # Total alkalinity ---------------------------------------------------------------------
  message(paste0("Magnesium hazard computed with: ", paste(present, collapse = ", ")))
  magnesium_hazard <- (dt$Mg_meq * 100) / (dt$Ca_meq + dt$Mg_meq)
  return(magnesium_hazard)
}
