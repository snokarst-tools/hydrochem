#' Replace Values Below Detection Limit (BDL)
#'
#' Detects and replaces values below detection limits (e.g., `"<0.01"`, `"< 0.005"`, `"< .5"`, `" < 0.1 "`) in a hydrochemical dataset.
#' Only columns including at least one BDL value are processed. Values are replaced with a numeric approximation based on the specified method.
#'
#' @param data A `data.frame` or `data.table`.
#' @param cols A character vector of cols names to summarize. If `NULL`, columns are detected automatically.
#' @param method Character. Method used to replace values below detection limits. One of:
#'   - `"DL/2"` (default): replaces with half the detection limit,
#'   - `"DL/sqrt(2)"`: replaces with DL divided by \eqn{\sqrt{x}}2,
#'   - `"DL"`: replaces with the detection limit itself,
#'   - `"0"`: replaces with zero.
#'   - `"NA"`: replaces with NA.
#'
#' @return A `data.table` where BDL character values have been replaced by numeric values according to the selected method.
#' @export
#'
#' @examples
#' df <- data.frame(Ca = c(" <0.01", "5.2", "< 0.005"), Cl = c("3.1", "<0.02", "2.5"))
#' df <- replace_bdl(df, method = "DL/2")
#' print(df)
#' 
#' data <- replace_bdl(data_qc, method = "0")
#' print(data)


replace_bdl <- function(data, 
                        cols = NULL,
                        method = c("DL/2", "DL/sqrt(2)", "DL", "0", "NA")) {
  
  # Get function arguments  -------------------------------------------------
  method <- match.arg(method)
  
  # Convert to data.table ---------------------------------------------------
  data <- as.data.table(data)
  
  # Get cols ----------------------------------------------------------------
  if (is.null(cols)) {
    cols <- names(data)[vapply(data, function(x) is.numeric(x) || is.character(x), logical(1))]
  } else {
    miss <- setdiff(cols, names(data))
    if (length(miss)) warning(paste0("Column ", paste(miss, collapse = ", "), " does not exist.\n"), call. = FALSE)
    cols <- intersect(cols, names(data))
  }
  
  # Change value of below detection limit -----------------------------------
  replaced_cols <- character(0)
  
  for (col in cols) {
    
    x <- data[[col]]
    
    # numeric columns: nothing to do (already numeric)
    if (is.numeric(x)) next
    
    # only try converting character columns that contain at least one "<..."
    if (!is.character(x)) next
    
    # Detect values below detection limit
    bdl_idx <- grepl("^\\s*<\\s*\\d*\\.?\\d+\\s*$", data[[col]])
    if (!any(bdl_idx)) next
    
    replaced_cols <- c(replaced_cols, col)
    
    # Extract numeric detection limits
    dl_vals <- as.numeric(sub("^\\s*<\\s*", "", trimws(data[[col]][bdl_idx])))
    
    # Apply method
    replacement <- switch(method,
                          "DL/2" = dl_vals / 2,
                          "DL/sqrt(2)" = dl_vals / sqrt(2),
                          "DL" = dl_vals,
                          "0" = 0,
                          "NA" = NA_real_
    )
    
    # Replace and convert whole column to numeric
    data[[col]][bdl_idx] <- replacement
    data[[col]] <- as.numeric(data[[col]])
  }
  
  if (length(replaced_cols)) {
    message("BDL replacement applied to: ", paste(unique(replaced_cols), collapse = ", "))
  }
  
  return(data)
}