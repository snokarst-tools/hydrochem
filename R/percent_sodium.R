#' Calculate Percent Sodium (%Na)
#'
#' Computes the Percent Sodium (%Na) index from hydrochemical data using sodium,
#' potassium, calcium, and magnesium concentrations. Input values may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#'
#' The function checks that all required ions (`Na`, `K`, `Ca`, `Mg`) are present
#' in the input dataset and throws an error if any are missing.
#'
#' @param data A `data.frame` or `data.table` containing the required ion
#'   columns `"Na"`, `"K"`, `"Ca"`, and `"Mg"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#'
#' @return A numeric vector of Percent Sodium values (in %), one per row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' percent_sodium(hc_data, base_unit = "mg/L")
#'
#' @import data.table

percent_sodium <- function(data,
                           base_unit = c("mg/L", "mmol/L", "meq/L")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  
  # Get ions ----------------------------------------------------------------
  ions <- c("Na", "K", "Ca", "Mg")
  needed <- c("Na", "K", "Ca", "Mg")
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
  message(paste0("Percent sodium computed with: ", paste(present, collapse = ", ")))
  percent_sodium <- (dt$Na_meq / (dt$Ca_meq + dt$Mg_meq + dt$Na_meq + dt$K_meq)) * 100
  return(percent_sodium)
}
