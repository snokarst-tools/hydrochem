gmean <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)] else return(NA_real_)
  if (any(x < 0)) return(NA_real_)
  if (any(x == 0)) return(0)
  exp(mean(log(x)))
}
