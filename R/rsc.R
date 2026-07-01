#' Calculate Residual Sodium Carbonate (RSC)
#' 
#'
#' Computes the Residual Sodium Carbonate (RSC) index from hydrochemical data
#' using calcium, magnesium, bicarbonate, and carbonate concentrations.
#' Input concentrations may be provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"`, and are internally converted to `"meq/L"`
#' before calculation.
#'
#' The function checks that all required ions (`CO3`, `HCO3`, `Ca`, `Mg`) are
#' present in the input dataset and throws an error if any are missing. If carbonate (`CO3`) is also provided, its contribution is added
#' to bicarbonate after conversion to milliequivalents (`meq/L`). If `CO3` is absent, alkalinity is considered to be represented solely by bicarbonate (`HCO3`).
#'
#' @param data A `data.frame` or `data.table` containing the required ion
#'   columns `"HCO3"`, `"Ca"`, and `"Mg"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#'
#' @return A numeric vector of Residual Sodium Carbonate values, one per row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' rsc(hc_data, base_unit = "mg/L")
#'
#' @import data.table

rsc <- function(data,
                base_unit = c("mg/L", "mmol/L", "meq/L")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  
  # Get ions ----------------------------------------------------------------
  ions <- c("HCO3", "Ca", "Mg")
  needed <- c("HCO3", "Ca", "Mg")
  
  # Add CO3 to anions if present in the dataset 
  if ("CO3" %in% names(data)) {
    ions <- c(ions, "CO3")
    needed <- c(needed, "CO3")
  } else {
    warning("CO3 is missing", call. = FALSE)
  }
  
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
  
  # Add CO3 to HCO3 if present
  if ("CO3_meq" %in% names(dt)) {
    dt$HCO3_meq <- dt$HCO3_meq + dt$CO3_meq
  } 
  
  # Total alkalinity ---------------------------------------------------------------------
  message(paste0("RSC computed with: ", paste(present, collapse = ", ")))
  rsc <- (dt$HCO3_meq) - (dt$Ca_meq + dt$Mg_meq)
  return(rsc)
}
