#' Determine Water Type
#'
#' Classifies each sample in a hydrochemical dataset by water type based on the dominant cations and anions.
#' Input concentrations may be
#' provided in `"mg/L"`, `"mmol/L"`, or `"meq/L"` and are internally converted
#' to `"meq/L"` prior to calculation.
#' 
#' Two formats are available:
#' - `"short"`: returns the single most dominant cation and anion (e.g., `"Ca-HCO3"`).
#' - `"long"`: returns all ions above a specified threshold, joined by `"+"` (e.g., `"Ca+Mg-HCO3+SO4"`).
#'
#' @param data A `data.frame` or `data.table` with columns named after ions (e.g., `"Ca"`, `"Cl"`, `"SO4"`).
#' @param base_unit Unit of the input ion concentrations. One of `"mg/L"`, `"mmol/L"`, or `"meq/L"`. Default is `"mg/L"`.
#' @param format Output format: `"short"` for dominant ions only, `"long"` to include all ions above a threshold. Default is `"long"`.
#' @param threshold Numeric. Minimum percentage for an ion to be considered dominant in `"long"` format. Default is `20`.
#'
#' @return A character vector of water types sorted in decreasing order, one per row of `data`.
#' @export
#'
#' @examples
#' data("hc_data", package = "hydrochem")
#' water_type(hc_data, base_unit = "mg/L", format = "short")
#' water_type(hc_data, base_unit = "mg/L", format = "long", threshold = 25)

water_type <- function(data,
                       base_unit = c("mg/L", "mmol/L", "meq/L"),
                       format = c("long", "short"), 
                       threshold = 20) {
  
  # Get function arguments  -------------------------------------------------
  base_unit <- match.arg(base_unit)
  format <- match.arg(format)
  
  # Get ions ----------------------------------------------------------------
  datacol <- names(data)
  cations <- datacol[datacol %in% ions_meta$ion_r[ions_meta$type == "cation"]]
  anions <- datacol[datacol %in% ions_meta$ion_r[ions_meta$type == "anion"]]
  ions <- c(cations, anions)
  
  # Prepare percent data ----------------------------------------------------
  data <- data |> 
    convert_unit(ions = ions, base_unit = base_unit, to_unit = "meq/L")
  
  data$total_cat <- rowSums(data[, paste0(cations, "_meq"), with = FALSE], na.rm = TRUE)
  data$total_an <- rowSums(data[, paste0(anions, "_meq"), with = FALSE], na.rm = TRUE)
  
  for (ion in ions) {
    col <- paste0(ion, "_meq")
    type <- if (ion %in% cations) "cat" else "an"
    data[[paste0(ion, "_meq_p")]] <- 100 * data[[col]] / data[[paste0("total_", type)]]
  }
  
  # Identify cations and anions above threshold -----------------------------
  cat_p <- data[, paste0(cations, "_meq_p"), with = FALSE]
  an_p  <- data[, paste0(anions, "_meq_p"), with = FALSE]
  
  if (format == "short") {
    cat_p[is.na(cat_p)] <- -Inf
    an_p[is.na(an_p)] <- -Inf
    max_cat <- cations[max.col(cat_p, ties.method = "first")]
    max_an  <- anions[max.col(an_p,  ties.method = "first")]
    wt <- paste(max_cat, max_an, sep = "-")
    return(wt)
  }
  
  get_dominants <- function(row, names) {
    above <- which(row > threshold)
    if (length(above) == 0) return(NA_character_)
    dom <- names[above[order(row[above], decreasing = TRUE)]]
    paste(dom, collapse = "-")
  }
  
  dom_cat <- apply(cat_p, 1, get_dominants, names = gsub("_meq_p", "", names(cat_p)))
  dom_an  <- apply(an_p,  1, get_dominants, names = gsub("_meq_p", "", names(an_p)))
  
  # Combine into water type -------------------------------------------------
  wt <- ifelse(is.na(dom_cat) | is.na(dom_an), NA_character_, paste(dom_cat, dom_an, sep = "-"))
  
  return(wt)
}
