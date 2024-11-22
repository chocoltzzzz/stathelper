solveODE <- function(a, b, c, f = NULL) {
  discriminant <- b^2 - 4 * a * c
  if (discriminant > 0) {
    r1 <- (-b + sqrt(discriminant)) / (2 * a)
    r2 <- (-b - sqrt(discriminant)) / (2 * a)
    yc <- paste0("C1 * exp(", round(r1, 2), " * x) + C2 * exp(", round(r2, 2), " * x)")
  } else if (discriminant == 0) {
    r <- -b / (2 * a)
    yc <- paste0("C1 * exp(", round(r, 2), " * x) + C2 * x * exp(", round(r, 2), " * x)")
  } else {

    real_part <- -b / (2 * a)
    imag_part <- sqrt(-discriminant) / (2 * a)
    yc <- paste0("exp(", round(real_part, 2), " * x) * (C1 * cos(",
                 round(imag_part, 2), " * x) + C2 * sin(", round(imag_part, 2), " * x))")
  }

  yp <- NULL
  if (!is.null(f)) {
    if (is.numeric(f)) {
      B <- f / c
      yp <- as.character(round(B, 2))
    } else if (grepl("x", f) && !grepl("exp", f)) {
      coeff <- as.numeric(sub("x", "", f))
      A <- coeff / c
      B <- -(b * A) / c
      yp <- paste0(round(A, 2), " * x + ", round(B, 2))
    } else if (grepl("exp", f)) {
      matches <- regmatches(f, regexec("([0-9.]+)exp\\^?\\(([0-9.-]+)x\\)", f))
      if (length(matches[[1]]) > 2) {
        k <- as.numeric(matches[[1]][3])
        const <- as.numeric(matches[[1]][2])
        denom <- a * k^2 + b * k + c
        A <- const / denom
        yp <- paste0(round(A, 2), " * exp(", k, " * x)")
      } else {
        yp <- "Invalid exponential forcing function format"
      }
    } else if (grepl("sin|cos", f)) {
      k <- as.numeric(sub(".*\\(", "", sub("\\).*", "", f)))
      denom <- a * k^2 + c
      A <- 1 / denom
      yp <- paste0(round(A, 2), " * sin(", k, " * x)")
    } else {
      yp <- "Unsupported forcing function"
    }
  } else {
    yp <- "0 (homogeneous equation)"
  }

  list(
    Complementary_Solution = yc,
    Particular_Solution = yp
  )
}
