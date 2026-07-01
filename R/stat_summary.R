#' Summary Statistics
#'
#' Computes summary statistics for selected numeric columns in a dataset, including:
#' mean, standard deviation, minimum, maximum, median, percentiles (1%, 5%, 25%, 75%, 95%, 99%),
#' geometric mean, mode, median absolut deviation (MAD), coefficient of variation (CV), range, interquartile range (IQR),
#' number of non-missing values (n), percentage of non-missing values (n_percent), number of NAs (NA), and percentage  of NAs (NA_percent).
#'
#' @param data A `data.frame` or `data.table` with numeric columns.
#' @param params A character vector of cols names to summarize. If `params = "all"`, all numerical columns are used.
#' @param group Optional character string. Name of the grouping column in `data`. Default is NULL.
#' @param stats Character vector of statistics to select (e.g., \code{c("min", "max", "mean")}).
#'   Must be among: \code{c("min", "P1", "P5", "P10", "P25", "med", "P75", "P90", "P95", "P99",
#'   "max", "mean", "gmean", "mode", "range", "IQR", "sd", "mad", "CV", "n", "n_percent",
#'   "NA", "NA_percent")}. If \code{NULL}, all available statistics are returned.
#' @param na.rm Logical. Should missing values be removed before computing statistics? Default is `FALSE`.
#'
#' @return A `data.table` with summarized statistics for the selected columns.
#' 
#' @export
#' 
#' @examples
#' data("hc_data", package = "hydrochem")
#' stat_summary(hc_data, params = c("Ca", "Mg", "Na"))
#' stat_summary(hc_data, group = "type", params = c("Ca", "Mg", "Na")) 
#' 
#' @import data.table

stat_summary <- function(data,
                         params = "all",
                         group = NULL,
                         stats = NULL,
                         na.rm = FALSE) {
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Get ions ----------------------------------------------------------------
  if (length(params) == 1 && params == "all") {
    params <- names(data)[sapply(data, is.numeric)]
  } else {
    # Check if cols exist
    check_cols <- params %in% names(data)
    if (any(!check_cols)) stop(paste0("Column ", params[!check_cols], " does not exist.\n"), call. = FALSE)
  }
  
  by_vars <- c(group, "param")
  
  # Melt the data ------------------------------------------------------------
  melted <- melt(
    data,
    id.vars = group,
    measure.vars = params,
    variable.name = "param",
    value.name = "value"
  )
  
  # Calculate statistics ----------------------------------------------------
  stats_dt <- melted[
    , .(
      min = min(value, na.rm = na.rm),
      P1 = na_quantile(value, 0.01, na.rm = na.rm),
      P5 = na_quantile(value, 0.05, na.rm = na.rm),
      P10 = na_quantile(value, 0.10, na.rm = na.rm),
      P25 = na_quantile(value, 0.25, na.rm = na.rm),
      med = median(value, na.rm = na.rm),
      P75 = na_quantile(value, 0.75, na.rm = na.rm),
      P90 = na_quantile(value, 0.90, na.rm = na.rm),
      P95 = na_quantile(value, 0.95, na.rm = na.rm),
      P99 = na_quantile(value, 0.99, na.rm = na.rm),
      max = max(value, na.rm = na.rm),
      mean = mean(value, na.rm = na.rm),
      gmean = gmean(value, na.rm = na.rm),
      mode = mode(value, na.rm = na.rm),
      range = max(value, na.rm = na.rm) - min(value, na.rm = na.rm),
      IQR = na_quantile(value, 0.75, na.rm = na.rm) - na_quantile(value, 0.25, na.rm = na.rm),
      sd = sd(value, na.rm = na.rm),
      mad = mad(value, na.rm = na.rm),
      CV = sd(value, na.rm = na.rm) / mean(value, na.rm = na.rm),
      n = sum(!is.na(value)),
      n_percent = (sum(!is.na(value)) / length(value)) * 100,
      `NA` = sum(is.na(value)),
      NA_percent = (sum(is.na(value)) / length(value)) * 100
    ),
    by = by_vars
  ]
  
  # Select stats if not null ------------------------------------------------
  if (!is.null(stats)) {
    valid <- c("min", "P1", "P5", "P10", "P25", "med", 
               "P75", "P90", "P95", "P99", "max", "mean", 
               "gmean", "mode", "range", "IQR", "sd", "mad", 
               "CV", "n", "n_percent", "NA", "NA_percent")
    invalid <- setdiff(stats, valid)
    if (length(invalid) > 0) stop("Invalid stats: ", paste(invalid, collapse = ", "), call. = FALSE)
    stats <- c(group, "param", stats)
    return(stats_dt[, ..stats])
  }
  
  return(stats_dt)
}
