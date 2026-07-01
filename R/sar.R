#' Calculate Sodium Adsorption Ratio (SAR)
#'
#' Computes the Sodium Adsorption Ratio (SAR) from hydrochemical data using
#' sodium, calcium, and magnesium concentrations. Input concentrations may be provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"`, and are internally converted to `"meq/L"`
#' before calculation.
#'
#' @param data A `data.frame` or `data.table` containing the required ion
#'   columns `"Na"`, `"Ca"`, and `"Mg"`.
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`,
#'   `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#'
#' @return A numeric vector of Sodium Adsorption Ratio values, one per row of
#'   `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' sar(hc_data, base_unit = "mg/L")
#'
#' @import data.table

sar <- function(data,
                base_unit = c("mg/L", "mmol/L", "meq/L")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  
  # Get ions ----------------------------------------------------------------
  ions <- c("Na", "Ca", "Mg")
  needed <- c("Na", "Ca", "Mg")
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
  message(paste0("SAR computed with: ", paste(present, collapse = ", ")))
  sar <- dt$Na_meq / sqrt((dt$Ca_meq + dt$Mg_meq) / 2)
  return(sar)
}
