#' Calculate Chloro-Alkaline Index 1 (CAI-1)
#'
#' Computes the Chloro-Alkaline Index 1 (CAI-1) from hydrochemical data using
#' sodium, potassium, and chloride concentrations. Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#'
#' The function checks that all required ions (`Na`, `K`, `Cl`) are present in
#' the input dataset and throws an error if any are missing.
#'
#' @param data A `data.frame` or `data.table` containing the required ion
#'   columns `"Na"`, `"K"`, and `"Cl"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#'
#' @return A numeric vector of CAI-1 values, one per row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' cai1(hc_data, base_unit = "mg/L")
#'
#' @import data.table

cai1 <- function(data,
                 base_unit = c("mg/L", "mmol/L", "meq/L")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  
  # Get ions ----------------------------------------------------------------
  ions <- c("Na", "K", "Cl")
  needed <- c("Na", "K", "Cl")
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
  message(paste0("CAI1 computed with: ", paste(present, collapse = ", ")))
  cai1 <- (dt$Cl_meq - (dt$Na_meq + dt$K_meq)) / dt$Cl_meq
  return(cai1)
}
