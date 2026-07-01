#' Calculate Ionic Balance Error (%)
#'
#' Computes the ionic balance error for each row of a hydrochemical dataset.
#'
#' The ionic balance error is calculated in percent:
#' \deqn{IB = ((T_c - T_a) / (T_c + T_a)) * 100}
#' where \eqn{T_c} is the total concentration of cations in meq/L,
#' and \eqn{T_a} is the total concentration of anions in meq/L.
#'
#' @param data A `data.frame` or `data.table` with numeric columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param method A character string specifying which ions to include. `"major"` uses only major ions;
#' `"major_minor"` includes both major and minor ions.
#'
#' @return A numeric vector of ionic balance error (in %), one per row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' hc_data$ib <- ionic_balance(hc_data,
#'                             base_unit = "mg/L")
#'
#' @import data.table

ionic_balance <- function(data,
                          base_unit = c("mg/L", "mmol/L", "meq/L"),
                          method = c("major", "major_minor")) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  method <- match.arg(method)
  keep <- switch (method,
                  "major" = "major",
                  "major_minor" = c("major", "minor")
  )
  
  
  # Get ions ----------------------------------------------------------------
  valid_ions <- ions_meta$ion_r[ions_meta$BI %in% keep]
  ions <- names(data)[names(data) %in% valid_ions]
  
  # Check for missing ions
  missing <- ions_meta$ion_r[ions_meta$BI %in% keep & !(ions_meta$ion_r %in% ions)]
  if (length(missing) > 0) {
    needed <- c("Ca", "Mg", "Na", "K", "HCO3", "Cl", "SO4")
    missing_major <- needed[needed %in% missing]
    if (length(missing_major) > 0) stop("Missing obligatory ions: ", paste(missing_major, collapse = ", "), call. = FALSE)
    if ("CO3" %in% missing) warning("CO3 is missing", call. = FALSE)
  }
  
  # Get cations and anions
  types <- ions_meta$type[match(ions, ions_meta$ion_r)]
  cations <- ions[types == "cation"]
  anions  <- ions[types == "anion"]
  
  # Convert to meq/L --------------------------------------------------------
  dt <- convert_unit(
    data,
    ions = ions,
    base_unit = base_unit,
    to_unit = "meq/L"
  )
  
  # Compute total cations and anions ----------------------------------------
  T_c <- rowSums(dt[, paste0(cations, "_meq"), with = FALSE], na.rm = TRUE)
  T_a <- rowSums(dt[, paste0(anions, "_meq"), with = FALSE], na.rm = TRUE)
  
  # Ionic balance -----------------------------------------------------------
  message(paste0("Ionic balance computed with: ", paste(ions, collapse = ", ")))
  ib <- (T_c - T_a) / (T_c + T_a) * 100
  return(ib)
}
