mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x = x[!is.na(x)]
  } else return(NA_real_)
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}