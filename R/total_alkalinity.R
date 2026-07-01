#' Calculate Total Alkalinity
#'
#' Computes total alkalinity from hydrochemical data using bicarbonate/carbonate 
#' concentrations and pH. Input concentrations may be provided in `"mg/L"`,
#' `"mmol/L"`, or `"meq/L"` and are internally converted to
#' `"meq/L"` prior to calculation.
#' 
#' The function checks that both required variables (`HCO3`, `pH`) are present in the input dataset and throws an error if either are
#' missing. If carbonate (`CO3`) is also provided, its contribution is added
#' to bicarbonate after conversion to milliequivalents (`meq/L`). If `CO3` is absent, alkalinity is considered to be represented solely by bicarbonate (`HCO3`).
#'
#' @param data A `data.frame` or `data.table` containing the required columns
#'   `"HCO3"` and `"pH"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`. 
#' @param output Unit of the returned hardness.
#'   One of:
#'   \itemize{
#'     \item `"mg/L_CaCO3"` — milligrams per liter as CaCO3 (default)
#'     \item `"meq/L"` — milliequivalents per liter
#'   }
#'
#' @return A numeric vector of Total Alkalinity values (in mg/L as CaCO3), one per row of
#'   `data`.
#' @export
#'
#' @examples
#' data("data_qc", package = "hydrochem")
#' total_alkalinity(data_qc, base_unit = "mg/L")
#'
#' @import data.table

total_alkalinity <- function(data,
                             base_unit = c("mg/L", "mmol/L", "meq/L"),
                             output = c("mg/L_CaCO3", "meq/L")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  output <- match.arg(output)
  
  # Get ions ----------------------------------------------------------------
  ions <- c("HCO3")
  needed <- c("HCO3", "pH")
  
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
  message(paste0("Total alkalinity computed with: ", paste(present, collapse = ", ")))
  message(paste0("Output is in ", output))
  total_alkanility <- dt$HCO3_meq + 10 ^ (-14 - dt$pH) - 10 ^ (-dt$pH)
  if (output == "mg/L_CaCO3") total_alkanility <- total_alkanility * 50
  return(total_alkanility)
}
