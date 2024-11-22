double_integral_area <- function(f1, f2, lower_bound, upper_bound) {
  if (!is.function(f1) || !is.function(f2)) {
    stop("f1 and f2 must be valid functions!")
  }

  area_function <- function(x) {
    abs(f2(x) - f1(x))
  }

  result <- integrate(area_function, lower_bound, upper_bound)
  return(result$value)
}

