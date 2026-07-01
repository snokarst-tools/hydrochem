#' Calculate Total Hardness
#'
#' Computes total water hardness from hydrochemical data using calcium and
#' magnesium concentrations. Input
#' concentrations may be provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted to
#' `"mg/L"` prior to calculation.
#' 
#' The function checks that both required ions (`Ca`, `Mg`) are present in the
#' input dataset and throws an error if either is missing.
#'
#' @param data A `data.frame` or `data.table` containing the required ion
#'   columns `"Ca"` and `"Mg"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param output Unit of the returned hardness.
#'   One of:
#'   \itemize{
#'     \item `"mg/L_CaCO3"` — milligrams per liter as CaCO3 (default)
#'     \item `"fH"` — French degrees
#'     \item `"dH"` — German degrees
#'     \item `"e"` — English degrees
#'   }
#'
#' @return A numeric vector of Total Hardness values (in mg/L as CaCO3), one per
#'   row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' total_hardness(hc_data, base_unit = "mg/L")
#'
#' @import data.table

total_hardness <- function(data,
                           base_unit = c("mg/L", "mmol/L", "meq/L"),
                           output = c("mg/L_CaCO3", "fH", "dH", "e")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  output <- match.arg(output)
  
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
  
  # Total hardness ---------------------------------------------------------------------
  message(paste0("Total hardness computed with: ", paste(ions, collapse = ", ")))
  message(paste0("Output is in ", output))
  total_hardness <- 50 * dt$Ca_meq + 50 * dt$Mg_meq
  if (output == "fH") total_hardness <- total_hardness / 10
  if (output == "dH") total_hardness <- total_hardness / 17.848
  if (output == "e") total_hardness <- total_hardness / 14.3
  return(total_hardness)
}
