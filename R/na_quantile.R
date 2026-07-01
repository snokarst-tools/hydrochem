na_quantile <- function(x, probs, na.rm) {
  tryCatch(
    quantile(x, probs, na.rm = na.rm),
    error = function(e) {
      NA_real_
    }
  )
}