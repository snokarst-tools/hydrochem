#' Calculate Chloro-Alkaline Index 2 (CAI-2)
#'
#' Computes the Chloro-Alkaline Index 2 (CAI-2) from hydrochemical data using
#' sodium, potassium, chloride, sulfate, bicarbonate, nitrate, and carbonate
#' concentrations. Input concentrations may be provided in `"mg/L"`, `"mmol/L"`,
#' or `"meq/L"` and are internally converted to `"meq/L"` prior to calculation.
#'
#' The function checks that all required ions (`Na`, `K`, `Cl`, `SO4`, `HCO3`, `NO3`) are present in the input dataset and throws an error if any are
#' missing. If carbonate (`CO3`) is also provided, its contribution is added
#' to bicarbonate after conversion to milliequivalents (`meq/L`). If `CO3` is absent, alkalinity is considered to be represented solely by bicarbonate (`HCO3`).
#'
#' @param data A `data.frame` or `data.table` containing the required ion
#'   columns `"Na"`, `"K"`, `"Cl"`, `"SO4"`, `"HCO3"`, and `"NO3"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#'
#' @return A numeric vector of CAI-2 values, one per row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' cai2(hc_data, base_unit = "mg/L")
#'
#' @import data.table

cai2 <- function(data,
                 base_unit = c("mg/L", "mmol/L", "meq/L")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  
  # Get ions ----------------------------------------------------------------
  ions <- c("Na", "K", "Cl", "SO4", "HCO3", "NO3")
  needed <- c("Na", "K", "Cl", "SO4", "HCO3", "NO3")
  
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
  
  if ("NO3" %in% missing) {
    if ("NO3_N" %in% names(data)) {
      ions <- c("Na", "K", "Cl", "SO4", "HCO3", "NO3_N")
      missing <- setdiff(missing, "NO3")
      present <- c(present, "NO3_N")
    }
  }
  
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
  message(paste0("CAI2 computed with: ", paste(present, collapse = ", ")))
  cai2 <- (dt$Cl_meq - (dt$Na_meq + dt$K_meq)) / (dt$SO4_meq + dt$HCO3_meq + dt$NO3_meq)
  return(cai2)
}
